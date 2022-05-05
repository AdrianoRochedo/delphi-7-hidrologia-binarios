<?xml version="1.0" encoding="ISO-8859-1" ?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method = "html" omit-xml-declaration = "no"
  doctype-system = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
  doctype-public = "-//W3C//DTD XHTML 1.0 STRICT//EN"/>                


<!--
        Copyright 2002 Rochedo Software
-->

<xsl:template match="BeginDoc">
<![CDATA[
<html>
  <title>
]]>
  <value-of select="@title"/>
<![CDATA[
  </title>
<body>
]]>
</xsl:template>

<xsl:template match="EndDoc">
<![CDATA[
</body>
</html>
]]>
</xsl:template>

</xsl:stylesheet>