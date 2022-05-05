<?xml version="1.0" encoding="ISO-8859-1" ?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method = "html" omit-xml-declaration = "no"
  doctype-system = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
  doctype-public = "-//W3C//DTD XHTML 1.0 STRICT//EN"/>                

<!--
	Copyright 2001 Rochedo Software
-->

  <xsl:template match="Vector">
     <p align="center">
      <table border="1" CELLPADDING="5">
        <caption>
          <b><font size="4" color="#3300ff"><xsl:value-of select="../@Name"/></font></b>:
        </caption>
        <xsl:apply-templates/>
      </table>
     </p>
  </xsl:template>

  <xsl:template match="e">
      <td><xsl:apply-templates/></td>
  </xsl:template>

</xsl:stylesheet>
