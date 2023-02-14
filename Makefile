packages := ~/.nuget/packages

PHONY := build
build:
	@dotnet build

PHONY := run
run:
	@dotnet run

PHONY := parser
parser_module = Feint.Parser
parser_in = src/Library/Parser.fsy
parser_out = src/Library/Parser.fs
parser:
	dotnet $(packages)/fslexyacc/11.0.1/build/fsyacc/net6.0/fsyacc.dll \
	  $(parser_in) \
	  -o $(parser_out) \
	  --module $(parser_module)

PHONY := lexer
lexer_module = Feint.Lexer
lexer_in = src/Library/Lexer.fsl
lexer_out = src/Library/Lexer.fs
lexer:
	dotnet $(packages)/fslexyacc/11.0.1/build/fslex/net6.0/fslex.dll \
	  $(lexer_in) \
	  -o $(lexer_out) \
	  --module $(lexer_module) \
	  --unicode
