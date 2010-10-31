Dupla:
	João Machini de Miranda
	Pedro Morhy Borges Leal


Para compilar:
	$> cd code
	$> make
	
	
Para levantar o servidor:
	$> make chat_server


Para rodar um cliente:
	$> erl -noshell -run chat_client start <nickname> -sname <nome_do_nó>
	

Como usar:
	- Compile a aplicação
	- Levante o servidor
	- Rode um cliente
		- Nesse momento uma tela com titulo "GroupList <nickname>" abrirá
		- A lista do centro mostra os grupos abertos
		- Digite o nome de um dos grupos da lista para acessá-lo ou um nome inexistente para criar um novo grupo
	- Dentro de um chat basta mandar mensagens para que todos do grupo a recebam
	- Se você desejar enviar uma mensagem para uma pessoa do grupo (sem que os outros membros do grupo a veja),
		basta digitar /to <nick_da_pessoa_desejada> <mensagem>
	- Se você fechar a tela de chat, voltará para a tela de seleção de grupo
	- Se você chegar a tela de seleção de grupo, sairá da aplicação cliente
