<?xml version="1.0" encoding="ISO-8859-1" ?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method = "html" omit-xml-declaration = "no"
  doctype-system = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
  doctype-public = "-//W3C//DTD XHTML 1.0 STRICT//EN"/>                

<!--
	Copyright 2001 Rochedo Software
-->

  <xsl:template match="Matrix">
     <p align="center">
        <xsl:apply-templates/>
     </p>
  </xsl:template>

  <xsl:template match="Data">
      <font face="verdana" size="1">
      <table bgcolor="#f5ebdb" border="1" cellPadding="5">
        <caption align="left">
          <b><font size="1" color="#3300ff"><xsl:value-of select="../@Name"/></font></b>:
          <xsl:value-of select="../@Label"/>

          <!-- Descrição das Variáveis -->
          <xsl:if test="../@PrintDescVars = '1'">
<!--            <br/> -->
            <br/>
            <table cellPadding="2">
              <xsl:for-each select="Variables/Variable">
                <xsl:if test="Label">
                  <tr>
                    <td width="20"/>
                    <td align="right"><b><xsl:value-of select="Name"/>:</b></td>
                    <td width="5"/>
                    <td><xsl:value-of select="Label"/></td>
                  </tr>
                </xsl:if>
              </xsl:for-each>
            </table>
            <br/>
          </xsl:if>

        </caption>
      <xsl:apply-templates/>
      </table>
      </font>
  </xsl:template>

  <xsl:template match="Variables">
      <THEAD>
        <tr>
          <xsl:apply-templates select="IdentVar"/>
          <xsl:apply-templates select="Variable/Name"/>
        </tr>
      </THEAD>
  </xsl:template>

  <xsl:template match="IdentVar">
      <th bgcolor="Silver">
        <xsl:apply-templates/>
      </th>
  </xsl:template>

  <xsl:template match="Variable/Name">
      <th bgcolor="Silver">
        <xsl:apply-templates/>
      </th>
  </xsl:template>

  <xsl:template match="Cols">
      <THEAD>
        <tr>
          <th><xsl:value-of select="../../@Name"/></th>
          <xsl:apply-templates/>
        </tr>
      </THEAD>
  </xsl:template>

  <xsl:template match="Col">
      <th bgcolor="Silver">
        <xsl:apply-templates/>
      </th>
  </xsl:template>

  <xsl:template match="Data/row">
      <tr>
        <td bgcolor="Silver"><b><xsl:value-of select="@Label"/></b></td>
        <xsl:apply-templates/>
      </tr>
  </xsl:template>

  <xsl:template match="e">
      <td nowrap="1">
        <xsl:choose>
          <xsl:when test="@cor">
            <font color="{@cor}"><xsl:apply-templates/></font>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates/>
          </xsl:otherwise>
        </xsl:choose>
      </td>
  </xsl:template>

</xsl:stylesheet>
