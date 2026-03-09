#!/bin/bash

LOCALREPO="$HOME/localrepo"

mkdir -p "$LOCALREPO"
cd "$LOCALREPO" || exit

# Заходим на сайт и находим ссылки, в самом начале все версии, чтоб с парсингом не мучаться
URLS=(
    "http://snapshot.debian.org/archive/debian/20250711T144811Z/pool/main/h/htop/htop_3.4.1-5_amd64.deb"
    "http://snapshot.debian.org/archive/debian/20250426T024229Z/pool/main/h/htop/htop_3.4.1-4_amd64.deb"
    "http://snapshot.debian.org/archive/debian/20250414T210755Z/pool/main/h/htop/htop_3.4.1-3_amd64.deb"
    "http://snapshot.debian.org/archive/debian/20250411T205535Z/pool/main/h/htop/htop_3.4.1-2_amd64.deb"
    "http://snapshot.debian.org/archive/debian/20230206T092119Z/pool/main/h/htop/htop_3.2.2-1_amd64.deb"
)

for URL in "${URLS[@]}"; do
    wget -c "$URL"
done
