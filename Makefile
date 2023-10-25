
#
# Makefile
#

#
# Note: uses -p : if on windows and you have git isntalled, you can right click a folder and Git Bash Here
#

RGBDS				:= C:/rgbds

RGBDS_ASM			:= $(RGBDS)/rgbasm
RGBDS_LINK			:= $(RGBDS)/rgblink
RGBDS_FIX			:= $(RGBDS)/rgbfix

ASM_FLAGS			:= 
LINK_FLAGS			:= 

# -v	= validate logo/header/global checksum
# -C	= colour compatible	[ -C = colour only -c = colour compatible ]
# -j	= non japanese region flag
# -m ?	= mbc_type [0x1B = ROM+MBC5+RAM+BATT]		header=0x0147
# -p 0	= pad the file to a valid size using 0's	header=0x0148 <- will auto set the correct size
# -r ?	= ram_size									header=0x0149 <- HEX values: 00=0k 01=2k 02=8k 03=32k
# -n ?	= rom_version, usually 0					header=0x014C
# -t ?	= set title 15 characers max
FIX_FLAGS			:= -v -p 0xFF -C -j -r 0x2 -m 0x1B -t ASMPROJECTTITLE

PROJECT_NAME		:= ASMProject
PROJECT_EXT			:= gbc
SRC_DIR				:= src
INC_DIR				:= include
BUILD_DIR			:= build/objects
ASSETS_DIR			:= assets/generated
ASSET_MAPS_DIR		:= assets/generated/maps
FINAL_DIR			:= final
FINAL_TARGET		:= $(FINAL_DIR)/$(PROJECT_NAME)

$(info Building : $(PROJECT_NAME).$(PROJECT_EXT))

########################################

# list all source .asm files	: src/main.asm src/player.asm src/battle.asm
SOURCE_FILES		:= $(wildcard $(SRC_DIR)/*.asm) $(wildcard $(ASSETS_DIR)/*.asm) $(wildcard $(ASSET_MAPS_DIR)/*.asm)
SOURCE_FILES		+= $(wildcard $(SRC_DIR)/**/*.asm) $(wildcard $(ASSETS_DIR)/**/*.asm) $(wildcard $(ASSET_MAPS_DIR)/**/*.asm)

# list all include .inc files	: -iinclude/hardware.inc -iinclude/temp.inc
INCLUDE_FILES		:= $(addprefix -i, $(wildcard $(INC_DIR)/*.inc))
INCLUDE_FILES		+= $(addprefix -i, $(wildcard $(INC_DIR)/**/*.inc))

# list all include .bin files	: -iinclude/assets/generated/player.bin -iinclude/assets/generated/font.bin
BIN_FILES			:= $(addprefix -i, $(wildcard $(ASSETS_DIR)/*.bin)) $(addprefix -i, $(wildcard $(ASSET_MAPS_DIR)/*.bin))
BIN_FILES			+= $(addprefix -i, $(wildcard $(ASSETS_DIR)/**/*.bin)) $(addprefix -i, $(wildcard $(ASSET_MAPS_DIR)/**/*.bin))

# list all objects to input		: src/main.o src/player.o src/battle.o
OBJECTS_INPUT		:= $(SOURCE_FILES:.asm=.o)

# list all objects that output	: build/main.o build/player.o build/battle.o
# they are placed into the build folder by removing the previous directory (notdir)
# using notdir, then a new directory is prefixed (addprefix)
OBJECTS_OUTPUT		:= $(addprefix $(BUILD_DIR)/, $(notdir $(SOURCE_FILES:%.asm=/%.o)))

#$(info SOURCE_FILES = $(SOURCE_FILES))
#$(info INCLUDE_FILES = $(INCLUDE_FILES))
#$(info BIN_FILES = $(BIN_FILES))
#$(info OBJECTS_INPUT = $(OBJECTS_INPUT))
#$(info OBJECTS_OUTPUT = $(OBJECTS_OUTPUT))

########################################

%.o : %.asm
	@echo Compiling $<
	@$(RGBDS_ASM) $(ASM_FLAGS) $(INCLUDE_FILES) $(BIN_FILES) -o $(BUILD_DIR)/$(notdir $@) $<

$(FINAL_TARGET) : prepare $(OBJECTS_INPUT)
	@$(RGBDS_LINK) $(LINK_FLAGS) -o $@.$(PROJECT_EXT) -n $@.sym $(OBJECTS_OUTPUT)
	@$(RGBDS_FIX) $(FIX_FLAGS) $(FINAL_TARGET).$(PROJECT_EXT)

########################################

prepare:
	@mkdir -p $(FINAL_DIR)
	@mkdir -p $(BUILD_DIR)

all:
	$(FINAL_TARGET)

clean:
	rm -rf $(FINAL_DIR)
	rm -rf $(BUILD_DIR)
	rm -rf $(ASSETS_DIR)
.PHONY: clean

run:
	$(TARGET)
	$(EMULATOR) $(TARGET)
.PHONY: run