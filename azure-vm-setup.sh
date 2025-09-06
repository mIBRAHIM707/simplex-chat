#!/bin/bash
# SimplexChat Development Environment Setup for Azure VM
# Run this script on a fresh Ubuntu 22.04 LTS VM

set -e

echo "🚀 Setting up SimplexChat development environment..."

# Update system
sudo apt update && sudo apt upgrade -y

# Install essential tools
sudo apt install -y \
    curl \
    git \
    build-essential \
    pkg-config \
    libffi-dev \
    libgmp-dev \
    libssl-dev \
    libtinfo-dev \
    libsystemd-dev \
    zlib1g-dev \
    make \
    g++ \
    tmux \
    htop \
    unzip \
    wget

# Install Nix package manager
echo "📦 Installing Nix..."
sh <(curl -L https://nixos.org/nix/install) --daemon
source /home/$USER/.nix-profile/etc/profile.d/nix.sh

# Enable nix flakes
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf

# Install Java 17 for Android development
echo "☕ Installing Java 17..."
sudo apt install -y openjdk-17-jdk
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
echo 'export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64' >> ~/.bashrc

# Install Android SDK
echo "📱 Installing Android SDK..."
cd ~
wget https://dl.google.com/android/repository/commandlinetools-linux-9477386_latest.zip
unzip commandlinetools-linux-9477386_latest.zip
mkdir -p ~/android-sdk/cmdline-tools
mv cmdline-tools ~/android-sdk/cmdline-tools/latest

# Set Android environment variables
export ANDROID_HOME=~/android-sdk
export ANDROID_SDK_ROOT=~/android-sdk
export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin:$ANDROID_HOME/platform-tools

echo 'export ANDROID_HOME=~/android-sdk' >> ~/.bashrc
echo 'export ANDROID_SDK_ROOT=~/android-sdk' >> ~/.bashrc
echo 'export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin:$ANDROID_HOME/platform-tools' >> ~/.bashrc

# Install Android SDK components
echo "📲 Installing Android SDK components..."
yes | $ANDROID_HOME/cmdline-tools/latest/bin/sdkmanager "platform-tools"
yes | $ANDROID_HOME/cmdline-tools/latest/bin/sdkmanager "platforms;android-34"
yes | $ANDROID_HOME/cmdline-tools/latest/bin/sdkmanager "build-tools;34.0.0"
yes | $ANDROID_HOME/cmdline-tools/latest/bin/sdkmanager "ndk;25.1.8937393"

# Clone SimplexChat
echo "📥 Cloning SimplexChat..."
cd ~
git clone https://github.com/mIBRAHIM707/simplex-chat.git
cd simplex-chat

echo "✅ Setup complete! Next steps:"
echo "1. Reboot the VM or run: source ~/.bashrc"
echo "2. cd ~/simplex-chat"
echo "3. nix develop"
echo "4. Start building!"
