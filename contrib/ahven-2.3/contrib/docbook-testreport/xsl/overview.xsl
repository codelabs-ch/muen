<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output type="xml"/>

	<xsl:variable name="total_suites" select="count(//testsuite)"/>
	<xsl:variable name="total_cases" select="count(//testcase)"/>
	<xsl:variable name="failed_cases" select="count(//failure)"/>

	<xsl:template match="/">
		<chapter id="overview">
			<title>Overview</title>
			<informaltable>
				<tgroup cols='2'>
					<tbody>
						<row>
							<entry>Total defined testsuites</entry>
							<entry><xsl:value-of select="$total_suites"/></entry>
						</row>
						<row>
							<entry>Total testcases executed</entry>
							<entry><xsl:value-of select="$total_cases"/></entry>
						</row>
						<row>
							<entry>Total failed testcases</entry>
							<entry><xsl:value-of select="$failed_cases"/></entry>
						</row>
					</tbody>
				</tgroup>
			</informaltable>
		</chapter>
	</xsl:template>

</xsl:stylesheet>
