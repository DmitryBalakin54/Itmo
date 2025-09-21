import os

import matplotlib.pyplot as plt
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
import torchvision
import torchvision.transforms as transforms
from imblearn.over_sampling import SMOTE
from torch.utils.data import DataLoader


def create_results_directories():
    datasets = ['stl10', 'mnist', 'fashion_mnist']
    for dataset in datasets:
        dataset_dir = os.path.join('results', dataset)
        if not os.path.exists(dataset_dir):
            os.makedirs(dataset_dir)
    return os.path.join('results', config.DATASET)


device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')


class Config:
    DATASET = 'stl10'  # 'stl10', 'mnist', 'fashion_mnist'

    BATCH_SIZE = 256
    IMG_SIZE = 96 if DATASET == 'stl10' else 28
    CHANNELS = 3 if DATASET == 'stl10' else 1

    LATENT_DIM = 128

    EPOCHS = 50
    LEARNING_RATE = 0.001
    BETA1 = 0.9
    BETA2 = 0.999

    NUM_VISUALIZATION_SAMPLES = 8
    SAVE_DPI = 300


config = Config()
results_dir = create_results_directories()

transform = transforms.Compose([
    transforms.Resize((config.IMG_SIZE, config.IMG_SIZE)),
    transforms.ToTensor(),
    transforms.Normalize((0.5,) * config.CHANNELS, (0.5,) * config.CHANNELS)
])


def get_dataset():
    if config.DATASET == 'stl10':
        return torchvision.datasets.STL10(
            root='./data', split='train', download=True, transform=transform)
    elif config.DATASET == 'mnist':
        return torchvision.datasets.MNIST(
            root='./data', train=True, download=True, transform=transform)
    elif config.DATASET == 'fashion_mnist':
        return torchvision.datasets.FashionMNIST(
            root='./data', train=True, download=True, transform=transform)
    else:
        raise ValueError(f"Dataset: {config.DATASET}")


train_dataset = get_dataset()
train_loader = DataLoader(train_dataset, batch_size=config.BATCH_SIZE, shuffle=True)


class Autoencoder(nn.Module):
    def __init__(self):
        super(Autoencoder, self).__init__()

        if config.DATASET == 'stl10':
            self.channels = [16, 32, 64, 128]
            self.spatial_sizes = [48, 24, 12, 6]
        else:
            self.channels = [16, 32, 64, 64]
            self.spatial_sizes = [14, 7, 4, 2]

        self.encoder = nn.Sequential(
            nn.Conv2d(config.CHANNELS, self.channels[0], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[0]),
            nn.ReLU(),

            nn.Conv2d(self.channels[0], self.channels[1], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[1]),
            nn.ReLU(),

            nn.Conv2d(self.channels[1], self.channels[2], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[2]),
            nn.ReLU(),

            nn.Conv2d(self.channels[2], self.channels[3], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[3]),
            nn.ReLU(),

            nn.Flatten()
        )

        self.encoder_output_size = self.channels[3] * self.spatial_sizes[3] * self.spatial_sizes[3]

        self.fc_encoder = nn.Linear(self.encoder_output_size, config.LATENT_DIM)
        self.fc_decoder = nn.Linear(config.LATENT_DIM, self.encoder_output_size)

        self.decoder = nn.Sequential(
            nn.ConvTranspose2d(self.channels[3], self.channels[2], kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.BatchNorm2d(self.channels[2]),
            nn.ReLU(),

            nn.ConvTranspose2d(self.channels[2], self.channels[1], kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.BatchNorm2d(self.channels[1]),
            nn.ReLU(),

            nn.ConvTranspose2d(self.channels[1], self.channels[0], kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.BatchNorm2d(self.channels[0]),
            nn.ReLU(),

            nn.ConvTranspose2d(self.channels[0], config.CHANNELS, kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.Tanh()
        )

    def forward(self, x):
        x = self.encoder(x)
        x = x.view(x.size(0), -1)
        latent = self.fc_encoder(x)
        
        x = self.fc_decoder(latent)
        x = x.view(x.size(0), self.channels[3], self.spatial_sizes[3], self.spatial_sizes[3])
        reconstructed = self.decoder(x)
        
        if config.DATASET == 'stl10':
            reconstructed = reconstructed[:, :, :96, :96]
        else:
            reconstructed = reconstructed[:, :, :28, :28]
            
        return reconstructed, latent


class VAE(nn.Module):
    def __init__(self):
        super(VAE, self).__init__()

        if config.DATASET == 'stl10':
            self.channels = [16, 32, 64, 128]
            self.spatial_sizes = [48, 24, 12, 6]
        else:
            self.channels = [16, 32, 64, 64]
            self.spatial_sizes = [14, 7, 4, 2]

        self.encoder = nn.Sequential(
            nn.Conv2d(config.CHANNELS, self.channels[0], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[0]),
            nn.ReLU(),

            nn.Conv2d(self.channels[0], self.channels[1], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[1]),
            nn.ReLU(),

            nn.Conv2d(self.channels[1], self.channels[2], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[2]),
            nn.ReLU(),

            nn.Conv2d(self.channels[2], self.channels[3], kernel_size=3, stride=2, padding=1),
            nn.BatchNorm2d(self.channels[3]),
            nn.ReLU(),

            nn.Flatten()
        )

        self.encoder_output_size = self.channels[3] * self.spatial_sizes[3] * self.spatial_sizes[3]

        self.fc_mu = nn.Linear(self.encoder_output_size, config.LATENT_DIM)
        self.fc_var = nn.Linear(self.encoder_output_size, config.LATENT_DIM)

        self.decoder_input = nn.Sequential(
            nn.Linear(config.LATENT_DIM, self.encoder_output_size),
            nn.ReLU()
        )

        self.decoder = nn.Sequential(
            nn.ConvTranspose2d(self.channels[3], self.channels[2], kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.BatchNorm2d(self.channels[2]),
            nn.ReLU(),

            nn.ConvTranspose2d(self.channels[2], self.channels[1], kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.BatchNorm2d(self.channels[1]),
            nn.ReLU(),

            nn.ConvTranspose2d(self.channels[1], self.channels[0], kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.BatchNorm2d(self.channels[0]),
            nn.ReLU(),

            nn.ConvTranspose2d(self.channels[0], config.CHANNELS, kernel_size=3, stride=2, padding=1, output_padding=1),
            nn.Tanh()
        )

    def encode(self, x):
        h = self.encoder(x)
        return self.fc_mu(h), self.fc_var(h)

    def reparameterize(self, mu, log_var):
        std = torch.exp(0.5 * log_var)
        eps = torch.randn_like(std)
        return mu + eps * std

    def decode(self, z):
        h = self.decoder_input(z)
        h = h.view(-1, self.channels[3], self.spatial_sizes[3], self.spatial_sizes[3])
        reconstructed = self.decoder(h)
        if config.DATASET == 'stl10':
            reconstructed = reconstructed[:, :, :96, :96]
        else:
            reconstructed = reconstructed[:, :, :28, :28]
        return reconstructed

    def forward(self, x):
        mu, log_var = self.encode(x)
        z = self.reparameterize(mu, log_var)
        return self.decode(z), mu, log_var


def denormalize(tensor):
    return (tensor + 1) / 2


def visualize_reconstructions(model, data_loader, filename, is_vae=False):
    model.eval()
    with torch.no_grad():
        images = []
        reconstructions = []

        for i, (data, _) in enumerate(data_loader):
            if i >= 4:
                break
            data = data.to(device)
            if is_vae:
                recon, _, _ = model(data)
            else:
                recon, _ = model(data)

            images.append(data)
            reconstructions.append(recon)

        images = torch.cat(images, dim=0)
        reconstructions = torch.cat(reconstructions, dim=0)

        images = denormalize(images)
        reconstructions = denormalize(reconstructions)

        # n_samples = len(images)
        n_samples = config.NUM_VISUALIZATION_SAMPLES
        for idx in range(n_samples):
            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 5))
            
            if config.CHANNELS == 1:
                ax1.imshow(images[idx].cpu().squeeze(), cmap='gray')
                ax2.imshow(reconstructions[idx].cpu().squeeze(), cmap='gray')
            else:
                img1 = images[idx].cpu().permute(1, 2, 0)
                img2 = reconstructions[idx].cpu().permute(1, 2, 0)
                img1 = torch.clamp(img1, 0, 1)
                img2 = torch.clamp(img2, 0, 1)
                ax1.imshow(img1)
                ax2.imshow(img2)

            ax1.set_title('Оригинал', fontsize=12)
            ax1.axis('off')

            ax2.set_title('Реконструкция', fontsize=12)
            ax2.axis('off')

            plt.tight_layout()
            plt.savefig(os.path.join(results_dir, f'{filename}_{idx + 1}.png'), dpi=config.SAVE_DPI,
                        bbox_inches='tight')
            plt.close()


def train_autoencoder(model, train_loader, epochs):
    model = model.to(device)
    criterion = nn.MSELoss()
    optimizer = optim.Adam(model.parameters(), lr=config.LEARNING_RATE,
                           betas=(config.BETA1, config.BETA2))
    scheduler = optim.lr_scheduler.ReduceLROnPlateau(optimizer, 'min', patience=3, factor=0.5)
    losses = []

    for epoch in range(epochs):
        model.train()
        total_loss = 0
        for batch_idx, (data, _) in enumerate(train_loader):
            data = data.to(device)
            optimizer.zero_grad()
            recon_batch, _ = model(data)
            loss = criterion(recon_batch, data)
            loss.backward()
            optimizer.step()
            total_loss += loss.item()

        avg_loss = total_loss / len(train_loader)
        losses.append(avg_loss)
        scheduler.step(avg_loss)
        print(f'Epoch [{epoch + 1}/{epochs}], Loss: {avg_loss:.4f}')

    return losses


def train_vae(model, train_loader, epochs):
    model = model.to(device)
    optimizer = optim.Adam(model.parameters(), lr=config.LEARNING_RATE,
                           betas=(config.BETA1, config.BETA2))
    scheduler = optim.lr_scheduler.ReduceLROnPlateau(optimizer, 'min', patience=3, factor=0.5)
    losses = []

    def loss_function(recon_x, x, mu, log_var):
        x = (x + 1) / 2
        
        batch_size = x.size(0)

        recon_loss = nn.functional.mse_loss(recon_x, x, reduction='sum') / batch_size
        
        KLD = -0.5 * torch.sum(1 + log_var - mu.pow(2) - log_var.exp()) / batch_size
        
        return recon_loss + 0.1 * KLD

    for epoch in range(epochs):
        model.train()
        total_loss = 0
        for batch_idx, (data, _) in enumerate(train_loader):
            data = data.to(device)
            optimizer.zero_grad()
            recon_batch, mu, log_var = model(data)
            loss = loss_function(recon_batch, data, mu, log_var)
            loss.backward()
            optimizer.step()
            total_loss += loss.item()

        avg_loss = total_loss / len(train_loader)
        losses.append(avg_loss)
        scheduler.step(avg_loss)
        print(f'Epoch [{epoch + 1}/{epochs}], Loss: {avg_loss:.4f}')

    return losses


def plot_learning_curve(losses, title, filename):
    plt.figure(figsize=(10, 5))
    plt.plot(losses)
    plt.title(title)
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.grid(True)
    plt.savefig(os.path.join(results_dir, f'{filename}.png'), dpi=config.SAVE_DPI, bbox_inches='tight')
    plt.close()


def generate_gaussian_samples(model, data_loader, n_samples=10):
    latents = []
    model.eval()
    with torch.no_grad():
        for data, _ in data_loader:
            data = data.to(device)
            if isinstance(model, VAE):
                latent, _ = model.encode(data)
            else:
                _, latent = model(data)
            latents.append(latent.cpu().numpy())
    
    latents = np.vstack(latents)
    mean = np.mean(latents, axis=0)
    cov = np.cov(latents.T)
    
    samples = np.random.multivariate_normal(mean, cov, n_samples)
    samples = torch.FloatTensor(samples).to(device)
    
    with torch.no_grad():
        if isinstance(model, VAE):
            h = model.decoder_input(samples)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        else:
            h = model.fc_decoder(samples)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        
        if config.DATASET == 'stl10':
            generated = generated[:, :, :96, :96]
        else:
            generated = generated[:, :, :28, :28]
    
    return generated


def generate_smote_samples(model, data_loader, n_samples=10):
    latents = []
    labels = []
    model.eval()
    with torch.no_grad():
        for data, target in data_loader:
            data = data.to(device)
            if isinstance(model, VAE):
                latent, _ = model.encode(data)
            else:
                _, latent = model(data)
            latents.append(latent.cpu().numpy())
            labels.append(target.numpy())
    
    latents = np.vstack(latents)
    labels = np.concatenate(labels)
    
    smote = SMOTE()
    latents_resampled, labels_resampled = smote.fit_resample(latents, labels)
    
    indices = np.random.choice(len(latents_resampled), n_samples)
    new_latents = torch.FloatTensor(latents_resampled[indices]).to(device)
    
    with torch.no_grad():
        if isinstance(model, VAE):
            h = model.decoder_input(new_latents)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        else:
            h = model.fc_decoder(new_latents)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        
        if config.DATASET == 'stl10':
            generated = generated[:, :, :96, :96]
        else:
            generated = generated[:, :, :28, :28]
    
    return generated, labels_resampled[indices]


def generate_conditional_samples(model, data_loader, class_idx, n_samples=10):
    class_latents = []
    model.eval()
    with torch.no_grad():
        for data, target in data_loader:
            mask = (target == class_idx)
            if mask.any():
                data = data[mask].to(device)
                if isinstance(model, VAE):
                    latent, _ = model.encode(data)
                else:
                    _, latent = model(data)
                class_latents.append(latent.cpu().numpy())
    
    if not class_latents:
        return None
    
    class_latents = np.vstack(class_latents)
    mean = np.mean(class_latents, axis=0)
    cov = np.cov(class_latents.T)
    
    samples = np.random.multivariate_normal(mean, cov, n_samples)
    samples = torch.FloatTensor(samples).to(device)
    
    with torch.no_grad():
        if isinstance(model, VAE):
            h = model.decoder_input(samples)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        else:
            h = model.fc_decoder(samples)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        
        if config.DATASET == 'stl10':
            generated = generated[:, :, :96, :96]
        else:
            generated = generated[:, :, :28, :28]
    
    return generated


def generate_mixed_class_samples(model, data_loader, n_samples=10):
    class_means = []
    class_covs = []
    
    model.eval()
    with torch.no_grad():
        for class_idx in range(10):
            class_latents = []
            for data, target in data_loader:
                mask = (target == class_idx)
                if mask.any():
                    data = data[mask].to(device)
                    if isinstance(model, VAE):
                        latent, _ = model.encode(data)
                    else:
                        _, latent = model(data)
                    class_latents.append(latent.cpu().numpy())
            
            if class_latents:
                class_latents = np.vstack(class_latents)
                class_means.append(np.mean(class_latents, axis=0))
                class_covs.append(np.cov(class_latents.T))
    
    mixed_samples = []
    for _ in range(n_samples):
        n_classes = np.random.randint(2, 4)
        class_indices = np.random.choice(len(class_means), n_classes, replace=False)
        
        mixed_latent = np.zeros_like(class_means[0])
        for idx in class_indices:
            weight = np.random.random()
            sample = np.random.multivariate_normal(class_means[idx], class_covs[idx])
            mixed_latent += weight * sample
        mixed_latent /= n_classes
        
        mixed_samples.append(mixed_latent)
    
    mixed_samples = torch.FloatTensor(np.array(mixed_samples)).to(device)
    
    with torch.no_grad():
        if isinstance(model, VAE):
            h = model.decoder_input(mixed_samples)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        else:
            h = model.fc_decoder(mixed_samples)
            if config.DATASET == 'stl10':
                h = h.view(-1, 128, 6, 6)
            else:
                h = h.view(-1, 64, 2, 2)
            generated = model.decoder(h)
        
        if config.DATASET == 'stl10':
            generated = generated[:, :, :96, :96]
        else:
            generated = generated[:, :, :28, :28]
    
    return generated


def visualize_generated_samples(generated, title, filename):
    plt.figure(figsize=(15, 5))
    for i in range(len(generated)):
        plt.subplot(1, len(generated), i + 1)
        if config.CHANNELS == 1:
            plt.imshow(generated[i].cpu().squeeze(), cmap='gray')
        else:
            img = generated[i].cpu().permute(1, 2, 0)
            img = torch.clamp(img, 0, 1)
            plt.imshow(img)
        plt.axis('off')
    plt.suptitle(title)
    plt.savefig(os.path.join(results_dir, f'{filename}.png'), dpi=config.SAVE_DPI, bbox_inches='tight')
    plt.close()


print(f"Используется датасет: {config.DATASET}")

print("Обучение автокодировщика...")
autoencoder = Autoencoder()
ae_losses = train_autoencoder(autoencoder, train_loader, config.EPOCHS)
plot_learning_curve(ae_losses, 'Кривая обучения автокодировщика', 'autoencoder_learning_curve')
visualize_reconstructions(autoencoder, train_loader, 'autoencoder_reconstructions')

print("\nГенерация изображений с помощью Гауссовой модели...")
gaussian_samples = generate_gaussian_samples(autoencoder, train_loader)
visualize_generated_samples(gaussian_samples, 'Гауссова модель', 'gaussian_samples')

print("\nГенерация изображений с помощью SMOTE...")
smote_samples, smote_labels = generate_smote_samples(autoencoder, train_loader)
visualize_generated_samples(smote_samples, 'SMOTE', 'smote_samples')

print("\nОбучение вариационного автокодировщика...")
vae = VAE()
vae_losses = train_vae(vae, train_loader, config.EPOCHS)
plot_learning_curve(vae_losses, 'Кривая обучения VAE', 'vae_learning_curve')
visualize_reconstructions(vae, train_loader, 'vae_reconstructions', is_vae=True)

print("\nУсловная генерация изображений...")
for class_idx in range(10):
    conditional_samples = generate_conditional_samples(vae, train_loader, class_idx)
    if conditional_samples is not None:
        visualize_generated_samples(conditional_samples, f'Класс {class_idx}', f'conditional_class_{class_idx}')

print("\nГенерация из смеси классов...")
mixed_samples = generate_mixed_class_samples(vae, train_loader)
visualize_generated_samples(mixed_samples, 'Смесь классов', 'mixed_class_samples')

print("\nГотово! Результаты сохранены в директории 'results'")







