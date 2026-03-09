#!/bin/bash

set -e

echo "Останавливаем и удаляем LVM..."

umount /mnt/vol01 2>/dev/null || true

lvremove -ff /dev/vg_data/lv_vol01 2>/dev/null || true
vgremove -ff vg_data 2>/dev/null || true

pvremove -ff /dev/sdc1 2>/dev/null || true
pvremove -ff /dev/sdd1 2>/dev/null || true
pvremove -ff /dev/sde1 2>/dev/null || true

echo "Стираем сигнатуры..."

wipefs -a /dev/sdc 2>/dev/null || true
wipefs -a /dev/sdd 2>/dev/null || true
wipefs -a /dev/sde 2>/dev/null || true

wipefs -a /dev/sdc1 2>/dev/null || true
wipefs -a /dev/sdd1 2>/dev/null || true
wipefs -a /dev/sde1 2>/dev/null || true

echo "Очистка завершена."
echo "Рекомендуется выполнить reboot."
