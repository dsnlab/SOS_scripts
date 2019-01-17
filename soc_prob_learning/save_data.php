<?php
// the $_POST[] array will contain the passed in filename and data
// the directory "data" is writable by the server (chmod 777)
// CHANGE THIS FILENAME SO THE PATH MAKES SENSE
$filename = "/experiment_data/yads_online/".$_POST['filename'];
$data = $_POST['filedata'];
// write the file to disk
file_put_contents($filename, $data);
?>
