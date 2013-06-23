<apply template="base">
  <bind tag="header">
    <h1>Change Your Name</h1>
  </bind>
  <bind tag="main">
	<form method="POST">
		<dfLabel ref="name">Name: </dfLabel>
		<dfInputText ref="name" />
	
		<dfInputSubmit />
	</form>
  </bind>
</apply>