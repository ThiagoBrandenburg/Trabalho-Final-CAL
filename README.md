# Trabalho-Final-CAL
 Trabalho Final de Complexidade de Algoritmos

DESCRIÇÃO

Este projeto desenvolvido por Thiago Brandenburg e Ana Veloy é o trabalho final da disciplina de Complexidade de Algoritmos, do curso de Ciência da Computação da Universidade do Estado de Santa Catarina (UDESC), ministrada pelo professor Cristiano Damiani Vasconcellos.

O trabalho na implementação do algoritmo de criptografia RSA e também o método de força bruta para obtenção da chave do mesmo. Para tal, foi utilizado Haskell, uma das linguagens disponibilizadas para a realização do trabalho.

REQUISITOS

-> Sistema Operacional Linux
-> Para medição dos tempos das funções, é necessário Interpretador do Haskell (Ghci)

EXECUÇÃO

Para executar o trabalho, basta utilizar o terminal no diretório do projeto e executar o comando *ghci trabalho_final.hs*, para medir o tempo de execução das funções é necessário passar o argumento *:set +s* dentro do ambiente do ghci.

FUNÇÕES PRINCIPAIS

main_chave Int = gera a chave publica e privada armazena seus respectivos arquivos .key. Os valores de p e q do rsa são gerados na ordem 2^(argumento -1), ou seja, o valor da chave pública é da ordem de 2 elevado ao parametro passado, visto que n = p*q

main_encripta = le o conteudo de texto_original.txt, encripta usando a chave publica e armazena o conteudo em texto_encriptado.txt

main_decripta = le o conteudo de texto_encriptado.txt, decripta usando a chave privada e armazena o resultado em texto_decriptado.txt

main_quebra = le a chave pública, obtem a tupla (p,q) por força bruta, utiliza a mesma para calcular a chave privada, usa a chave obtida para decriptar texto_encriptado.txt e armazena o resultado em texto_rsa_quebrado.txt

main_limpa = limpa os arquivos texto_encriptado, texto_decriptado, texto_rsa_quebrado
