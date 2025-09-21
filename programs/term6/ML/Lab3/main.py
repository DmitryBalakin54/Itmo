import io
import random
import sys
from collections import Counter
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader


class TextTokenizer:
    def __init__(self, text):
        sentences = [s.strip() for s in text.split('\n\n') if s.strip()]

        words = []
        for sentence in sentences:
            words.extend(sentence.lower().split())

        word_counts = Counter(words)
        self.vocab = {'<PAD>': 0, '<UNK>': 1}

        for word, count in word_counts.most_common():
            if count >= 2:
                self.vocab[word] = len(self.vocab)

        self.idx_to_word = {idx: word for word, idx in self.vocab.items()}
        self.sentences = sentences

    def encode(self, text):
        return [self.vocab.get(word.lower(), self.vocab['<UNK>']) for word in text.split()]

    def decode(self, indices):
        words = []
        for idx in indices:
            word = self.idx_to_word.get(idx, '')
            if word not in ['<PAD>', '<UNK>']:
                words.append(word)

        return ' '.join(words)


class TextDataset(Dataset):
    def __init__(self, tokenizer, seq_length=20):
        self.tokenizer = tokenizer
        self.seq_length = seq_length
        self.data = []

        for sentence in tokenizer.sentences:
            tokens = tokenizer.encode(sentence)

            if len(tokens) < seq_length + 1:
                continue

            for i in range(len(tokens) - seq_length):
                x_seq = tokens[i:i + seq_length]
                y_seq = tokens[i + 1:i + seq_length + 1]
                self.data.append((x_seq, y_seq))

    def __len__(self):
        return len(self.data)

    def __getitem__(self, idx):
        x, y = self.data[idx]
        return torch.tensor(x), torch.tensor(y)


class LSTMModel(nn.Module):
    def __init__(self, vocab_size, embedding_dim=128, hidden_dim=256, num_layers=2, dropout=0.2):
        super().__init__()
        self.embedding = nn.Embedding(vocab_size, embedding_dim)
        self.lstm = nn.LSTM(
            embedding_dim,
            hidden_dim,
            num_layers=num_layers,
            dropout=dropout if num_layers > 1 else 0,
            batch_first=True
        )
        self.fc = nn.Linear(hidden_dim, vocab_size)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x, hidden=None):
        x = self.embedding(x)
        lstm_out, hidden = self.lstm(x, hidden)
        x = self.dropout(lstm_out)
        output = self.fc(x)
        return output, hidden


class MarkovChain:
    def __init__(self, text, n=3):
        self.n = n
        self.chain = {}
        sentences = [s.strip() for s in text.split('\n\n') if s.strip()]
        self._build_chain(sentences)

    def _build_chain(self, sentences):
        for sentence in sentences:
            words = sentence.lower().split()
            if len(words) < self.n + 1:
                continue

            for i in range(len(words) - self.n):
                context = tuple(words[i:i + self.n])
                next_word = words[i + self.n]

                if context not in self.chain:
                    self.chain[context] = Counter()
                self.chain[context][next_word] += 1

    def generate(self, seed, length=50, temperature=1.0):
        if not seed:
            seed = random.choice(list(self.chain.keys()))
        else:
            words = seed.lower().split()
            if len(words) < self.n:
                seed = tuple(words + [''] * (self.n - len(words)))
            else:
                seed = tuple(words[-self.n:])

        result = list(seed)
        last_contexts = set()

        for _ in range(length):
            if seed not in self.chain:
                seed = random.choice(list(self.chain.keys()))
                result.extend(seed)
                continue

            next_words = self.chain[seed]
            if not next_words:
                break

            if tuple(result[-self.n:]) in last_contexts:
                seed = random.choice(list(self.chain.keys()))
                result.extend(seed)
                continue

            last_contexts.add(tuple(result[-self.n:]))
            if len(last_contexts) > 10:
                last_contexts.pop()

            total = sum(next_words.values())
            probs = {word: (count / total) ** (1 / temperature) for word, count in next_words.items()}
            total_prob = sum(probs.values())
            probs = {word: prob / total_prob for word, prob in probs.items()}

            next_word = random.choices(list(probs.keys()), weights=list(probs.values()))[0]
            result.append(next_word)
            seed = tuple(result[-self.n:])

        filtered_result = [word for word in result if word]
        return ' '.join(filtered_result).replace('  ', ' ').strip()


def train_model(model, train_loader, criterion, optimizer, num_epochs=30, device=None):
    model.train()

    for epoch in range(num_epochs):
        total_loss = 0
        for x, y in train_loader:
            x, y = x.to(device), y.to(device)

            optimizer.zero_grad()

            output, _ = model(x)

            output = output.reshape(-1, output.shape[-1])
            y = y.reshape(-1)

            loss = criterion(output, y)

            loss.backward()

            torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0)

            optimizer.step()

            total_loss += loss.item()

        avg_loss = total_loss / len(train_loader)
        print(f'Epoch {epoch + 1}/{num_epochs}, Loss: {avg_loss:.4f}')


def generate_text(model, tokenizer, seed_text, length=50, temperature=1.0, device=None):
    model.eval()
    with torch.no_grad():
        tokens = tokenizer.encode(seed_text)

        if len(tokens) < 20:
            tokens = [tokenizer.vocab['<PAD>']] * (20 - len(tokens)) + tokens

        context = tokens[-20:]
        generated = []

        if seed_text and seed_text.strip() and tokenizer.vocab.get(seed_text.lower(), -1) != tokenizer.vocab['<UNK>']:
            generated.extend(tokenizer.encode(seed_text))

        max_attempts = 150
        attempts = 0

        while len(generated) < length and attempts < max_attempts:
            attempts += 1

            x = torch.tensor([context]).to(device)

            output, _ = model(x)
            output = output[0, -1].squeeze()

            output = output / temperature

            if temperature < 1.0:
                for special_idx in [tokenizer.vocab['<PAD>'], tokenizer.vocab['<UNK>']]:
                    output[special_idx] = -float('inf')

                top_k = 5
                top_indices = torch.topk(output, top_k)[1]

                if random.random() < 0.9:
                    next_token_id = top_indices[0].item()
                else:
                    next_token_id = top_indices[random.randint(0, top_k - 1)].item()
            else:
                for special_idx in [tokenizer.vocab['<PAD>'], tokenizer.vocab['<UNK>']]:
                    output[special_idx] = -float('inf')

                probs = torch.softmax(output, dim=-1)

                top_k = min(20, len(probs))
                top_probs, top_indices = torch.topk(probs, top_k)

                if top_probs.sum().item() > 0:
                    next_token_id = top_indices[torch.multinomial(top_probs, 1)].item()
                else:
                    valid_tokens = [idx for idx, word in tokenizer.idx_to_word.items()
                                    if word not in ['<PAD>', '<UNK>']]
                    next_token_id = random.choice(valid_tokens) if valid_tokens else -1

                    if next_token_id == -1:
                        continue

            if next_token_id in [tokenizer.vocab['<PAD>'], tokenizer.vocab['<UNK>']]:
                continue

            generated.append(next_token_id)

            context = context[1:] + [next_token_id]

        result = tokenizer.decode(generated)
        if not result.strip():
            return "defeat"

        result = result.replace('  ', ' ').strip()
        for punctuation in [',', '.', '!', '?', ':', ';']:
            result = result.replace(' ' + punctuation, punctuation)

        return result


sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

with open('data.txt', 'r', encoding='utf-8') as f:
    text = f.read()

tokenizer = TextTokenizer(text)

dataset = TextDataset(tokenizer, seq_length=20)
train_loader = DataLoader(dataset, batch_size=32, shuffle=True)

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

model = LSTMModel(
    vocab_size=len(tokenizer.vocab),
    embedding_dim=128,
    hidden_dim=256,
    num_layers=2,
    dropout=0.2
).to(device)

criterion = nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(model.parameters(), lr=0.0005)

train_model(model, train_loader, criterion, optimizer, num_epochs=60, device=device)

with open('res.txt', 'w', encoding='utf-8') as f:
    lstm_greedy = generate_text(model, tokenizer, "Я", length=50, temperature=0.8, device=device)
    lstm_random = generate_text(model, tokenizer, "Я", length=50, temperature=1.2, device=device)

    markov = MarkovChain(text, n=3)
    markov_greedy = markov.generate("Я", length=50, temperature=0.8)
    markov_random = markov.generate("Я", length=50, temperature=1.2)

    f.write("Генерация текста LSTM (жадный алгоритм):\n")
    f.write(lstm_greedy + "\n\n")

    f.write("Генерация текста LSTM (случайный выбор):\n")
    f.write(lstm_random + "\n\n")

    f.write("Генерация текста Марковскими цепями (жадный алгоритм):\n")
    f.write(markov_greedy + "\n\n")

    f.write("Генерация текста Марковскими цепями (случайный выбор):\n")
    f.write(markov_random + "\n")
