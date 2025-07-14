#!/bin/bash
set -e

# Script pro pour builder et installer COTS CLI avec choix de la méthode
# Usage: ./build-and-install.sh [--method cabal|stack]

PROJECT_NAME="cardano-offline-transaction-simulator"
BIN_NAME="cotscli"
INSTALL_DIR="$HOME/.local/bin"
METHOD="stack"

# Couleurs
GREEN='\033[0;32m'
CYAN='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --method)
      METHOD="$2"
      shift 2
      ;;
    *)
      echo -e "${RED}Unknown argument: $1${NC}"
      exit 1
      ;;
  esac
done

printf "${CYAN}🔧 Build/install method: ${METHOD}${NC}\n"

if [ "$METHOD" = "cabal" ]; then
  if ! command -v cabal &> /dev/null; then
    printf "${RED}❌ cabal not found! Please install cabal or use --method stack.${NC}\n"
    exit 1
  fi
  printf "${CYAN}🔨 Building with cabal...${NC}\n"
  cabal build
  printf "${CYAN}🚚 Installing with cabal...${NC}\n"
  cabal install exe:${BIN_NAME} --installdir=${INSTALL_DIR} --overwrite-policy=always
elif [ "$METHOD" = "stack" ]; then
  if ! command -v stack &> /dev/null; then
    printf "${RED}❌ stack not found! Please install stack or use --method cabal.${NC}\n"
    exit 1
  fi
  printf "${CYAN}🔨 Building with stack...${NC}\n"
  stack build
  printf "${CYAN}🚚 Installing with stack...${NC}\n"
  stack install --local-bin-path ${INSTALL_DIR}
else
  printf "${RED}❌ Unknown method: $METHOD (use --method cabal|stack)${NC}\n"
  exit 1
fi

# Vérifier l'installation
if [ -x "${INSTALL_DIR}/${BIN_NAME}" ]; then
  printf "${GREEN}✅ $BIN_NAME installed successfully in ${INSTALL_DIR}/${BIN_NAME}${NC}\n"
else
  printf "${RED}❌ $BIN_NAME not found in ${INSTALL_DIR}!${NC}\n"
  exit 1
fi

# Afficher la version
printf "${CYAN}ℹ️  $BIN_NAME version:${NC}\n"
${INSTALL_DIR}/${BIN_NAME} version

printf "${GREEN}🎉 Build and install complete!${NC}\n" 