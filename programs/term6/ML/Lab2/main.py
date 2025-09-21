import os
import time

import matplotlib.pyplot as plt
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, Subset
from torchvision import datasets, transforms


def create_loader(dataset, shuffle, batch_size):
    return DataLoader(
        dataset,
        batch_size=batch_size,
        shuffle=shuffle,
        pin_memory=True,
        num_workers=0
    )


def train_model(model, train_loader, test_loader, epochs, lr, device, head='mnist'):
    model.to(device)
    optimizer = optim.AdamW(model.parameters(), lr=lr)
    criterion = nn.CrossEntropyLoss()

    history = {'train_loss': [], 'test_loss': [], 'train_acc': [], 'test_acc': []}

    for epoch in range(1, epochs + 1):
        start_time = time.time()

        model.train()
        train_loss, correct, total = 0, 0, 0
        for x, y in train_loader:
            x, y = x.to(device, non_blocking=True), y.to(device, non_blocking=True)

            optimizer.zero_grad(set_to_none=True)
            outputs = model(x, head=head)
            loss = criterion(outputs, y)
            loss.backward()
            optimizer.step()

            train_loss += loss.item()
            _, predicted = outputs.max(1)
            total += y.size(0)
            correct += predicted.eq(y).sum().item()

        train_loss /= len(train_loader)
        train_acc = correct / total

        test_loss, test_acc = evaluate(model, test_loader, criterion, device, head)

        history['train_loss'].append(train_loss)
        history['test_loss'].append(test_loss)
        history['train_acc'].append(train_acc)
        history['test_acc'].append(test_acc)

        print(f"Epoch {epoch}/{epochs} | Time: {time.time() - start_time:.1f}s | "
              f"Train Loss: {train_loss:.4f}, Acc: {train_acc:.2%} | "
              f"Test Loss: {test_loss:.4f}, Acc: {test_acc:.2%}")

    return history


def evaluate(model, loader, criterion, device, head):
    model.eval()
    loss, correct, total = 0, 0, 0

    with torch.no_grad():
        for x, y in loader:
            x, y = x.to(device), y.to(device)
            outputs = model(x, head=head)
            loss += criterion(outputs, y).item()

            _, predicted = outputs.max(1)
            total += y.size(0)
            correct += predicted.eq(y).sum().item()

    return loss / len(loader), correct / total


def save_plot(history, title, results_dir='results'):
    plt.figure(figsize=(12, 4))

    plt.subplot(1, 2, 1)
    plt.plot(history['train_loss'], label='Train')
    plt.plot(history['test_loss'], label='Test')
    plt.title(f'{title} Loss')
    plt.legend()

    plt.subplot(1, 2, 2)
    plt.plot(history['train_acc'], label='Train')
    plt.plot(history['test_acc'], label='Test')
    plt.title(f'{title} Accuracy')
    plt.legend()

    plt.tight_layout()
    os.makedirs(results_dir, exist_ok=True)
    plt.savefig(f'{results_dir}/{title.lower().replace(" ", "_")}.png')
    plt.close()


def find_similar_images(model, test_loader, device, num_examples=5, results_dir='results'):
    model.eval()
    features = []
    labels = []
    images = []

    with torch.no_grad():
        for x, y in test_loader:
            x = x.to(device)
            feat = model.features(x).view(x.size(0), -1).cpu()
            features.append(feat)
            labels.append(y)
            images.append(x.cpu())

    features = torch.cat(features)
    labels = torch.cat(labels)
    images = torch.cat(images)

    class_centroids = []
    for c in range(10):
        mask = labels == c
        if mask.any():
            class_centroids.append(features[mask].mean(0))
        else:
            class_centroids.append(None)

    class_names = ['T-shirt', 'Trouser', 'Pullover', 'Dress', 'Coat',
                   'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Boot']

    plt.figure(figsize=(20, 20), dpi=150)

    for target_class in range(10):
        for source_class in range(10):
            if target_class == source_class or class_centroids[target_class] is None:
                continue

            distances = torch.norm(features[labels == source_class] - class_centroids[target_class], dim=1)

            _, indices = torch.topk(distances, k=num_examples, largest=False)

            idx = target_class * 10 + source_class + 1
            plt.subplot(10, 10, idx)

            similar_imgs = torch.stack([images[labels == source_class][i] for i in indices[:5]])
            img_grid = torch.cat([img.squeeze() for img in similar_imgs], dim=1)

            plt.imshow(img_grid, cmap='gray', vmin=-1, vmax=1)
            plt.title(f"{class_names[source_class]}→{class_names[target_class]}", fontsize=6)
            plt.axis('off')

    plt.tight_layout()
    os.makedirs(results_dir, exist_ok=True)
    plt.savefig(f'{results_dir}/similar_images.png', dpi=300, bbox_inches='tight', pad_inches=0.1)
    plt.close()


class DualHeadCNN(nn.Module):
    def __init__(self):
        super().__init__()
        self.features = nn.Sequential(
            nn.Conv2d(1, 64, 3, padding=1),
            nn.BatchNorm2d(64),
            nn.ReLU(inplace=True),
            nn.MaxPool2d(2),
            nn.Conv2d(64, 128, 3, padding=1),
            nn.BatchNorm2d(128),
            nn.ReLU(inplace=True),
            nn.MaxPool2d(2),
            nn.AdaptiveAvgPool2d((4, 4))
        )
        self.mnist_head = nn.Linear(128 * 4 * 4, 10)
        self.fashion_head = nn.Linear(128 * 4 * 4, 10)

    def forward(self, x, head='mnist'):
        x = self.features(x)
        x = x.view(x.size(0), -1)
        if head == 'mnist':
            return self.mnist_head(x)
        return self.fashion_head(x)


torch.backends.cudnn.benchmark = True
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"Using device: {device}")

config = {
    'batch_size': 512,
    'lr': 3e-4,
    'epochs_mnist': 8,
    'epochs_fashion': 6,
    'epochs_finetune': 4,
    'num_similar_examples': 1,
    'results_dir': 'results'
}

transform = transforms.Compose([
    transforms.ToTensor(),
    transforms.Normalize((0.5,), (0.5,))
])

print("Loading datasets...")
mnist_train = datasets.MNIST(root='./data', train=True, download=True, transform=transform)
mnist_test = datasets.MNIST(root='./data', train=False, download=True, transform=transform)
fashion_train = datasets.FashionMNIST(root='./data', train=True, download=True, transform=transform)
fashion_test = datasets.FashionMNIST(root='./data', train=False, download=True, transform=transform)

if len(fashion_test) > 2000:
    fashion_test = Subset(fashion_test, range(2000))

mnist_train_loader = create_loader(mnist_train, True, config['batch_size'])
mnist_test_loader = create_loader(mnist_test, False, config['batch_size'])
fashion_train_loader = create_loader(fashion_train, True, config['batch_size'])
fashion_test_loader = create_loader(fashion_test, False, config['batch_size'])

print("\n=== Training MNIST ===")
model = DualHeadCNN()
mnist_history = train_model(
    model, mnist_train_loader, mnist_test_loader,
    config['epochs_mnist'], config['lr'], device, 'mnist'
)
torch.save(model.state_dict(), f"{config['results_dir']}/mnist_model.pth")

print("\n=== Training FashionMNIST (frozen) ===")
model_fashion = DualHeadCNN()
model_fashion.load_state_dict(torch.load(f"{config['results_dir']}/mnist_model.pth"))

for name, param in model_fashion.named_parameters():
    if 'fashion_head' not in name:
        param.requires_grad = False

fashion_history = train_model(
    model_fashion, fashion_train_loader, fashion_test_loader,
    config['epochs_fashion'], config['lr'], device, 'fashion'
)

print("\n=== Fine-tuning ===")
for param in model_fashion.parameters():
    param.requires_grad = True

finetune_history = train_model(
    model_fashion, fashion_train_loader, fashion_test_loader,
    config['epochs_finetune'], 1e-5, device, 'fashion'
)

mnist_loss, mnist_acc = evaluate(model_fashion, mnist_test_loader, nn.CrossEntropyLoss(), device, 'mnist')
print(f"\nMNIST after fine-tuning: Loss: {mnist_loss:.4f}, Accuracy: {mnist_acc:.2%}")

find_similar_images(model_fashion, fashion_test_loader, device,
                    config['num_similar_examples'], config['results_dir'])

save_plot(mnist_history, "MNIST", config['results_dir'])
save_plot(fashion_history, "FashionMNIST Frozen", config['results_dir'])
save_plot(finetune_history, "FashionMNIST Fine-tuned", config['results_dir'])

