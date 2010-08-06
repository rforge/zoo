<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="http://www.colegioamorim.com.br/ermelino/files/animals_hanging_around_zoo_sign_hg_wht.gif" border="0" alt="zoo Logo" /> </a> </td> </tr>
</table>

<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> The <strong>CRAN summary page</strong> can be found <a href="http://cran.r-project.org/web/packages/zoo/index.html"><strong>here</strong></a>. This page includes direct links to the documentation, NEWS, source, Windows binary and other elements of the project.</p>
<p> The <strong>zoo help pages</strong> can be found <a href="http://bm2.genes.nig.ac.jp/RGM2/pkg.php?p=zoo"><strong>here</strong></a>. </p>
<p> The <strong>zoo image gallery</strong> can be found <a href="http://bm2.genes.nig.ac.jp/RGM2/index.php?scope=name&query=zoo"><strong>here</strong></a>. </p>
<p> The <strong>R-Forge summary page</strong> can be found <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. This page contains links to the development source repository which is hosted under subversion.</p>

</body>
</html>
