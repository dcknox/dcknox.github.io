<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head id="Head1" runat="server">
    <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
    <title>Email Validation</title>
    <link rel="stylesheet" href="styles.css" type="text/css" />
</head>
<body>


    <div id="wrap">

        <div id="page">
        	<img src="logo.png">
            <h1>Title</h1><div id="main-content"> xxx

<?php

function update($email_id, $confirmed){
	$servername = "police.cluster-cltcxfxlqxjk.us-west-2.rds.amazonaws.com";
	$username = "admin";
	$password = "GAzK6Rojsp8fQkAsrLvf";
	$dbname = "police";

	$connection = new mysqli($servername, $username, $password, $dbname);
	// if ($conn->connect_error) {
	//     die("Connection failed: " . $conn->connect_error);
	// } 

	$sql = "insert into emails (email_id, confirmed) values('".$_GET["id"]."','".$_GET["confirmed"]."')";
	mysqli_query($connection, $sql);
	$connection->close();
}
update();

if($_GET["confirmed"] == "0"){
	print('<h1>Your name will be removed.</h1>
		<p>We apologize for the inconvenience.</p>

		');

	
}
if($_GET["confirmed"] == "1"){
	print('<h1>Thank you for your support.</h1>
		<p>Please encourage others to lend their voices at the following link: <a href="https://forms.gle/MY9xFA24tGnWWL447">https://forms.gle/MY9xFA24tGnWWL447</a></p>
		');
			
}

?>
</div>
<div class="clear">
</div>
</div>
</div>
</form>

</body>
</html>
