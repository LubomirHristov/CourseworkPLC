#
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#

# Files that need to be generated from other files
DEPEND += Tokens.hs Grammar.hs Types.hs Eval.hs

# When "make" is invoked with no arguments, we build an executable
#  after building everything that it depends on
all: $(DEPEND) myinterpreter

# Build an executable for Toy interpreter
myinterpreter: $(DEPEND) myinterpreter.hs
	ghc myinterpreter.hs

# Generate ML files from a parser definition file
Grammar.hs : Grammar.y
	@rm -f Grammar.hs
	happy Grammar.y
	@chmod -w Grammar.hs

# Generate ML files from a lexer definition file
Tokens.hs : Tokens.x
	@rm -f Tokens.hs
	alex Tokens.x
	@chmod -w Tokens.hs

# Clean up the directory
clean::
	rm -rf Tokens.hs Grammar.hs *.hi *.o *.info
