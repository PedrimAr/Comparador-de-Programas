# Alvo padrão: compila o código
all:
	ghc -O2 -o similaridade main.hs

# Alvo para executar
run: all
	./similaridade res.txt sep.txt c1.txt c2.txt

# Alvo para limpar os binários
clean:
	rm -f *.o *.hi similaridade