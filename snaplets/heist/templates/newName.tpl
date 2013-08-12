<apply template="base">
    <bind tag="page_title">
        Change Your Name
    </bind>
    <bind tag="main">
        <div class="main-content">
            <dfChildErrorList ref="" />
            <form method="POST" class="form-horizontal">
                <div class="form-group">
                    <dfLabel ref="name" class="col-lg-2 control-label">New Name: </dfLabel>
                    <div class="col-lg-10">
                        <dfInputText class="form-control" placeholder="Enter your new name" ref="name" />
                    </div>
                </div>
                
                <div class="form-group">
                    <dfInputSubmit class="btn btn-default"/>
                </div>
            </form>
        </div>
    </bind>
</apply>