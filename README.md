# Elimina/Conclui itens de requisições de compras
 Programa que elimina/conclui itens das Requisições de Compra informadas.  
 
 Ao final do processamento é exibido log de registros alterados salvos na tabela ZMMT010.  
 
 Programa útil para eliminação/conclusão de requisições de compra em massa.  
 
## Exemplo de uso

Informar o número de uma requisição de compra:
<img src="tela de selecao.png" align="center">

É exibida a tela com todos os itens das requisições de compra encontrados:
<img src="tela principal.png" align="center">

Ao ser clicado no botão **Concluir Req.Compra** o programa processa o item da requisição e marca como concluído através da função **BAPI_PR_CHANGE**, sendo exibido a tela de log:
<img src="tela log concluido.png" align="center">
 
Ao voltar para a tela principal a coluna "Concluída" fica marcada:
<img src="tela principal flag concluido.png" align="center">

Ao ser clicado no botão **Eliminar Req.Compra** o programa processa o item da requisição e marca como eliminado através da função **BAPI_PR_CHANGE**, sendo exibido a tela de log:
<img src="tela log eliminado.png" align="center">

Ao voltar para a tela principal a coluna "CódElim" fica marcada:
<img src="tela principal flag eliminado.png" align="center">

**Obs:** o log fica salvo na tabela ZMMT010.
 
