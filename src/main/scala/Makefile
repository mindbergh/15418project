ifndef ($(SCALA_HOME))
	SCALA_HOME=$(dir $(shell which scalac))..
endif

COMPILER_FILES=$(wildcard plugin/*.scala)
TEST_FILES=$(wildcard test/*.scala)
PWD=$(shell pwd)

SCALAC=$(SCALA_HOME)/bin/scalac
SCALA=$(SCALA_HOME)/bin/scala


ARCH_NAME=$(shell uname)
ifeq ($(ARCH_NAME),Darwin)
	FSC=$(SCALA_HOME)/bin/fsc
else
	FSC=$(SCALAC)
endif

BUILD_DIR=build
CLASS_DIRECTORY=$(BUILD_DIR)/classes
OUTPUT_DIRECTORY=$(BUILD_DIR)/outputs
CLASS_PATH=$(CLASS_DIRECTORY)

PLUGIN=$(BUILD_DIR)/cuSparkplugin.jar
COMPILER_FLAGS=-deprecation -unchecked

.PHONY: all plugin test

all: plugin test


plugin: $(COMPILER_FILES)
	$(FSC) $(COMPILER_FLAGS) -classpath $(CLASS_PATH) -d $(CLASS_DIRECTORY) $(COMPILER_FILES)
	cp plugin/scalac-plugin.xml $(CLASS_DIRECTORY)/scalac-plugin.xml
	(cd $(CLASS_DIRECTORY); jar cf $(PWD)/$(PLUGIN) .)

test:	$(TEST_FILES)
	$(SCALAC) $(COMPILER_FLAGS) -classpath $(CLASS_PATH) -Xplugin:$(PLUGIN) -d $(OUTPUT_DIRECTORY) $(TEST_FILES) && touch $(CLASS_DIRECTORY)/test.txt
