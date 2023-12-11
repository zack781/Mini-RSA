MAKE=@make
DUNE=@dune
LN=@ln -sf
RM=@rm
BIN=rsa

.PHONY: test

all:
	$(DUNE) build src/main.exe
	$(LN) _build/default/src/main.exe $(BIN)

test: all
	$(DUNE) test

promote:
	$(DUNE) promote

clean:
	$(DUNE) clean
	$(RM) -rf $(BIN)
