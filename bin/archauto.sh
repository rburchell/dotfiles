# !/bin/bash
set -e

DISK=/dev/sda

echo "WARNING: THIS SCRIPT WILL TRASH $DISK"
echo "To begin, press enter."
echo "Arch will be installed and configured, and $DISK will be toast!"
read

echo "Creating partitions"
parted -a optimal --script ${DISK} -- mklabel msdos
parted -a optimal --script ${DISK} -- mkpart primary ext4 1M 100%
parted -a optimal --script ${DISK} -- set 1 boot on

echo "Applying a file system to ${DISK}1 ..."
mkfs.ext4 ${DISK}1

#echo "Applying a file system to ${DISK}3 ..."
#mkfs.ext4 ${DISK}3

#echo "Creating the swap partition ..."
#mkswap ${DISK}2
#
#echo "Activating the swap partition ..."
#swapon ${DISK}2

echo "Mounting the partitions ..."
mount ${DISK}1 /mnt
#mkdir /mnt/home
#mount ${DISK}3 /mnt/home

echo "Installing the base system ..."
pacstrap /mnt base base-devel

echo "Generating a fstab ..."
genfstab /mnt >> /mnt/etc/fstab

#echo "Please ensure that the fstab is OK!"
#nano /mnt/etc/fstab

echo "Setting up the chroot ..."
echo "Enter a hostname for your device ..."
read HOSTNAME
echo ${HOSTNAME} > /mnt/etc/hostname

# locale, see /etc/locale.gen to modify
echo "en_US.UTF-8 UTF-8" > /mnt/etc/locale.gen
arch-chroot /mnt locale-gen

echo "Setting up the bootloader..."
arch-chroot /mnt pacman -S grub-bios --noconfirm
arch-chroot /mnt grub-install ${DISK}
arch-chroot /mnt mkinitcpio -p linux
arch-chroot /mnt grub-mkconfig -o /boot/grub/grub.cfg

echo "Installing additional stuff ..."
arch-chroot /mnt pacman -S git --noconfirm
arch-chroot /mnt pacman -S vim --noconfirm
arch-chroot /mnt pacman -S openssh --noconfirm
arch-chroot /mnt pacman -S avahi --noconfirm
arch-chroot /mnt pacman -S zsh --noconfirm

echo "Enabling services ..."
arch-chroot /mnt systemctl enable dhcpcd.service
arch-chroot /mnt systemctl enable sshd.service
arch-chroot /mnt systemctl enable avahi-daemon.service

echo "Setting up root ..."
echo "Enter a new root password ..."
arch-chroot /mnt passwd
arch-chroot /mnt chsh -s /bin/zsh root
arch-chroot /mnt git clone https://github.com/rburchell/dotfiles.git /root
arch-chroot /mnt chown -R root:root /root

echo "Setting up user account ..."
echo "Enter a new user password ..."
arch-chroot /mnt useradd burchr
arch-chroot /mnt passwd burchr
mkdir -p /mnt/home/burchr
arch-chroot /mnt git clone https://github.com/rburchell/dotfiles.git /home/burchr/
arch-chroot /mnt chown -R burchr:burchr /home/burchr
arch-chroot /mnt chsh -s /bin/zsh burchr
echo "burchr ALL=NOPASSWD: ALL" > /mnt/etc/sudoers.d/burchr

echo "Unmounting and cleaning up ..."
#umount /mnt/home 
umount /mnt

echo "Install complete. Press enter to reboot."
read
reboot
