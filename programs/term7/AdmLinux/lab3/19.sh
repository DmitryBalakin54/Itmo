#!/bin/bash

# Создаём рабочий каталог для пересборки пакета nano
mkdir -p ~/nano_rebuild
cd ~/nano_rebuild || exit 1

# Скачиваем пакет nano из репозитория
apt download nano

# Распаковываем содержимое пакета в каталог nano_mod
dpkg-deb -R nano_*.deb nano_mod

# Создаём каталог usr/bin, если он не существует
mkdir -p nano_mod/usr/bin

# Создаём символическую ссылку newnano → /usr/bin/nano внутри пакета
ln -s /usr/bin/nano nano_mod/usr/bin/newnano

# Пересобираем пакет
dpkg-deb --build nano_mod newnano.deb

# Устанавливаем пересобранный пакет
dpkg -i newnano.deb
