################################################################################
## MIT License
##
## Copyright (c) 2020 Zane Myers
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
################################################################################
# This Makefile is used to build, install, and update the GlfwAda library, as
# well as to build its tests.
################################################################################

.PHONY: default tests compile install clean uninstall

default: tests

DIRS = obj bin lib

$(DIRS):
	@echo "Making Directory $@..."
	mkdir -p $@
	@echo "   Done."

compile: $(DIRS)
	@echo "Compiling GLFWAda..."
	gprbuild -p glfwada.gpr
	@echo "   Done."

install: compile
	@echo "Installing GLFWAda..."
	gprinstall -p -f -v glfwada.gpr
	@echo "   Done."

uninstall:
	@echo "Uninstalling GLFWAda..."
	gprinstall --uninstall glfwada
	@echo "   Done."

tests: $(DIRS)
	@echo "Compiling GLFWAda Tests..."
	gprbuild -p glfwada-test.gpr
	cp dependencies/glfw3.dll ./bin
	./bin/glfw_test-environment.exe
	@echo "   Done."

clean:
	@echo "Cleaning Directories..."
	rm -rf $(DIRS)
	@echo "   Done."
