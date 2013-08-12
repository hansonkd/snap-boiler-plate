<apply template="base">
    <bind tag="page_title">
        Login with OpenID
    </bind>
    <bind tag="extra_css">
        <link type="text/css" rel="stylesheet" href="/static/css/openid.css" />
    </bind>
    <bind tag="main">
        <div class="main-content">
        	<form method="POST" id="openid_form">
        		<input type="hidden" name="action" value="verify" />
        		<fieldset>
        			<legend>Sign-in or Create New Account</legend>
        			<div id="openid_choice">
        				<p>Please click your account provider:</p>
        				<div id="openid_btns"></div>
        			</div>
        			<div id="openid_input_area">
        				<input id="openid_identifier" name="openid_identifier" type="text" value="http://" />
        				<input id="openid_submit" type="submit" value="Sign-In"/>
        			</div>
        			<noscript>
        				<p>OpenID is service that allows you to log-on to many different websites using a single indentity.
        				Find out <a href="http://openid.net/what/">more about OpenID</a> and <a href="http://openid.net/get/">how to get an OpenID enabled account</a>.</p>
        			</noscript>
        		</fieldset>
        	</form>
        </div>
    </bind>
    <bind tag="extra_js">
    	<script type="text/javascript" src="/static/js/openid-jquery.js"></script>
    	<script type="text/javascript" src="/static/js/openid-en.js"></script>
    	<script type="text/javascript">
    		$(document).ready(function() {
                openid.img_path = '/static/images/';
    			openid.init('openid_identifier');
    		});
    	</script>
    </bind>
</apply>

