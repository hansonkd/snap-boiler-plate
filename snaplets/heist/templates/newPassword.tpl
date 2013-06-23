<apply template="base">
  <bind tag="header">
    <h1>Change Your Passphrase</h1>
  </bind>
  <bind tag="main">
   <dfChildErrorList ref="" />
	<form method="POST">
		<dfLabel ref="originalPass">Old Passphrase: </dfLabel>
		<dfInputText ref="originalPass" />
		
		<dfLabel ref="passphrase.p1">New Passphrase: </dfLabel>
		<dfInputPassword ref="passphrase.p1" />
		
		<dfLabel ref="passphrase.p2">Repeat: </dfLabel>
		<dfInputPassword ref="passphrase.p2" />
	
		<dfInputSubmit />
	</form>
  </bind>
</apply>