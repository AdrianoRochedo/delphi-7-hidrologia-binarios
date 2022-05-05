<?xml version="1.0" encoding="ISO-8859-1" ?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method = "html" omit-xml-declaration = "no"
  doctype-system = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
  doctype-public = "-//W3C//DTD XHTML 1.0 STRICT//EN"/>                

<!--
	Copyright 2001 Rochedo Software
-->

  <xsl:template match="p">
      <p align="justify">
      <xsl:apply-templates/>
      </p>
  </xsl:template>

  <xsl:template match="center">
      <center><xsl:apply-templates/></center>
  </xsl:template>

  <xsl:template match="legend">
     <center>
       <table bgcolor="#f3e8bb" border="1" cellpadding="10">
       <tr>
       <td>
          <b><xsl:value-of select="@title"/>:</b>
          <table border="0">
             <xsl:apply-templates/>
          </table>
       </td>
       </tr>
       </table>
     </center>
  </xsl:template>

  <xsl:template match="legend/item">
     <tr>
         <td width="20"/>
         <td width="4" bgcolor="{@color}"></td>
         <td width="5"/>
         <td><xsl:apply-templates/></td>
     </tr>
  </xsl:template>

  <xsl:template match="PropertyValue">
     <font face="verdana" size="1">
     <table bgcolor="silver" width="100%" border="{@border}">
       <tr>
         <td width="{@propPercent}" align="right"><b><xsl:value-of select="@property"/></b></td>
         <td width="2"></td>
         <td valign="top"><xsl:value-of select="@value"/></td>
       </tr>
     </table>
     </font>
  </xsl:template>

  <xsl:template match="title1">
      <table width="100%">
        <tr>
          <td align="center" bgcolor="#0600e2">
            <font size="3" color="#ffffff"><xsl:apply-templates/></font>
          </td>
        </tr>
      </table>
  </xsl:template>

  <xsl:template match="title2">
      <H2><font face="verdana" size="3"><xsl:apply-templates/></font></H2>
  </xsl:template>

  <xsl:template match="title3">
      <H3><i><font face="verdana" size="2"><xsl:apply-templates/></font></i></H3>
  </xsl:template>

  <xsl:template match="title4">
      <H4><i><xsl:apply-templates/></i></H4>
  </xsl:template>

  <xsl:template match="NewLine">
      <BR/>
  </xsl:template>

  <xsl:template match="warning">
      <font color="blue"><b>Aviso: </b></font>
      <font color="red"><xsl:apply-templates/></font><br/>
  </xsl:template>

  <xsl:template match="b">
      <b><xsl:apply-templates/></b>
  </xsl:template>

  <xsl:template match="i">
      <i><xsl:apply-templates/></i>
  </xsl:template>

  <xsl:template match="br">
      <br/>
  </xsl:template>

  <xsl:template match="text">
    <table>
      <tr>
      <td width="20"></td>
      <td><xsl:apply-templates/></td>
      <td width="20"></td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="itemlist">
     <ul><xsl:apply-templates/></ul>
  </xsl:template>

  <xsl:template match="item">
     <li><xsl:apply-templates/></li>
  </xsl:template>

  <xsl:template match="red">
     <font color="red"><xsl:apply-templates/></font>
  </xsl:template>

  <xsl:template match="green">
     <font color="green"><xsl:apply-templates/></font>
  </xsl:template>

  <xsl:template match="blue">
     <font color="blue" size="1"><xsl:apply-templates/></font>
  </xsl:template>

  <xsl:template match="gray">
     <font color="gray"><xsl:apply-templates/></font>
  </xsl:template>

  <!-- Regras para Formatação de Tabelas -->

  <xsl:template match="Table">
     <font face="verdana" size="1">
     <table bgcolor="#f5ebdb" border="{@border}" align="{@align}" cellpadding="4">
       <xsl:apply-templates/>
     </table>
     </font>
  </xsl:template>

  <xsl:template match="headers">
     <thead>
       <tr>
         <xsl:apply-templates/>
       </tr>
     </thead>
  </xsl:template>

  <xsl:template match="header">
     <th bgcolor="Silver">
       <xsl:apply-templates/>
     </th>
  </xsl:template>

  <xsl:template match="row">
     <tr>
       <xsl:apply-templates/>
     </tr>
  </xsl:template>

  <xsl:template match="cell">
     <td>
        <xsl:choose>
          <xsl:when test="@bold">
            <b><xsl:apply-templates/></b>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates/>
          </xsl:otherwise>
        </xsl:choose>
     </td>
  </xsl:template>

  <!-- Fim das Regras para Formatação de Tabelas -->

  <xsl:template match="link">
     <center>
     <script language="JavaScript"></script>
     <strong><font face="verdana" size="1">
       <a href="#"
          onclick="window.open('{@ref}')"
          onmouseover = "this.style.fontSize = '15'; this.style.color = 'red'"
          onmouseout = "this.style.fontSize =''; this.style.color = ''">
       <xsl:apply-templates/>
       </a>
     </font></strong>
     </center>
  </xsl:template>

</xsl:stylesheet>
