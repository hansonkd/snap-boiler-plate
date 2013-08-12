<apply template="base">
    <bind tag="page_title">
        Change Your Passphrase
    </bind>
    <bind tag="main">
        <div class="main-content">
            <dfChildErrorList ref="" />
            <form method="POST" class="form-horizontal">
                <div class="form-group">
                    <dfLabel ref="originalPass" class="col-lg-2 control-label">Old Passphrase: </dfLabel>
                    <div class="col-lg-10">
                        <dfInputPassword class="form-control" placeholder="Enter original passphrase" ref="originalPass" />
                    </div>
                </div>
                <div class="form-group">
                    <dfLabel ref="passphrase.p1" class="col-lg-2 control-label">New Passphrase: </dfLabel>
                    <div class="col-lg-10">
                        <dfInputPassword class="form-control" placeholder="Enter new passphrase" ref="passphrase.p1" />
                    </div>
                </div>
                <div class="form-group">
                    <dfLabel ref="passphrase.p2" class="col-lg-2 control-label">Repeat: </dfLabel>
                    <div class="col-lg-10">
                        <dfInputPassword class="form-control" placeholder="Repeat passphrase" ref="passphrase.p2" />
                    </div>
                </div>
                
                <div class="form-group">
                    <dfInputSubmit class="btn btn-default"/>
                </div>
            </form>
        </div>
    </bind>
</apply>