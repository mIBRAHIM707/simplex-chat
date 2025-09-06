#!/bin/bash
# SimplexChat Build Script for Azure VM
# Run this after setting up the development environment

set -e

echo "🔨 Building SimplexChat with Read Receipts..."

# Ensure we're in the right directory
cd ~/simplex-chat

# Enter nix development environment
echo "🐚 Entering Nix development environment..."
nix develop --command bash << 'EOF'

echo "📚 Building Haskell backend..."
# Build the Haskell chat library
cabal build simplex-chat

echo "🔧 Building libsimplex for Android..."
# Build the shared library for Android
cabal build --constraint="simplex-chat +swift" simplex-chat

# Extract the built library
LIB_PATH=$(cabal list-bin --constraint="simplex-chat +swift" simplex-chat)
echo "Library built at: $LIB_PATH"

# Copy to Android jniLibs (you'll need to adjust paths based on actual build output)
mkdir -p apps/multiplatform/android/src/main/jniLibs/arm64-v8a
mkdir -p apps/multiplatform/android/src/main/jniLibs/armeabi-v7a
mkdir -p apps/multiplatform/android/src/main/jniLibs/x86_64
mkdir -p apps/multiplatform/android/src/main/jniLibs/x86

# Note: You may need to build for different architectures
# This is a simplified version - actual cross-compilation setup needed

echo "📱 Building Android APK..."
cd apps/multiplatform
./gradlew assembleDebug

echo "✅ Build complete!"
echo "📦 APK location: apps/multiplatform/android/build/outputs/apk/debug/"

EOF
