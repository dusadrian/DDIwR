cacheEnv <- new.env()
assign(
    "DDIC",
    list(
        abstract = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language used in the title. Use of ISO (www.iso.org) language codes is recommended.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                date = list(
                    type = "xs:string",
                    description = "Date of writing the abstract, should follow ISO convention of YYYY-MM-DD.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                contentType = list(
                    type = "xs:string",
                    description = "Provides forward-compatibility with DDI Lifecycle by describing where the content fits in that structure, or if is \"mixed\" in terms of what is contained.",
                    values = c("abstract", "purpose", "mixed"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "stdyInfo",
            children = c(),
            title = "Abstract",
            description = "An unformatted summary describing the purpose, nature, and scope of the data collection, special characteristics of its contents, major subject areas covered, and what questions the PIs attempted to answer when they conducted the study. A listing of major variables in the study is important here. In cases where a codebook contains more than one abstract (for example, one might be supplied by the data producer and another prepared by the data archive where the data are deposited), the \"source\" and \"date\" attributes may be used to distinguish the abstract versions. Maps to Dublin Core element \"Description\". Inclusion of this element in the codebook is recommended.",
            examples = "<abstract date=\"1999-01-28\" source=\"ICPSR\" contentType=\"abstract\" xml:lang=\"en\">Data on labor force activity for the week prior to the survey are supplied in this collection. Information is available on the employment status, occupation, and industry of persons 15 years old and over. Demographic variables such as age, sex, race, marital status, veteran status, household relationship, educational background, and Hispanic origin are included. In addition to providing these core data, the May survey also contains a supplement on work schedules for all applicable persons aged 15 years and older who were employed at the time of the survey. This supplement focuses on shift work, flexible hours, and work at home for both main and second jobs.</abstract>"
        ),
        accsPlac = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                URI = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "setAvail",
            children = c(),
            title = "Location of Data Collection",
            description = "Location where the data collection is currently stored. Use the URI attribute to provide a URN or URL for the storage site or the actual address from which the data may be downloaded.",
            examples = "<accsPlac URI=\"https://international.ipums.org\">IPUMS International</accsPlac>"
        ),
        actMin = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c(),
            title = "Actions to Minimize Losses",
            description = "Summary of actions taken to minimize data loss. Includes information on actions such as follow-up visits, supervisory checks, historical matching, estimation, etc.",
            examples = "<actMin>To minimize the number of unresolved cases and reduce the potential nonresponse bias, four follow-up contacts were made with agencies that had not responded by various stages of the data collection process.</actMin>"
        ),
        add = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "fileDerivationVars",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        algorithmSpecification = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "dataFingerprint",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        algorithmVersion = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "dataFingerprint",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        altTitl = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "titlStmt",
            children = c(),
            title = "Alternative Title",
            description = "A title by which the work is commonly referred, or an abbreviation of the title.",
            examples = "<altTitl>PISA</altTitl>"
        ),
        anlyInfo = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "method",
            children = c("respRate", "EstSmpErr", "dataAppr"),
            title = "Data Appraisal",
            description = "Information on data appraisal.",
            examples = "<anlyInfo>Data meets FAIR criteria.</anlyInfo>"
        ),
        anlysUnit = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("nCube", "var"),
            children = c(),
            title = "Analysis Unit",
            description = "Provides information regarding whom or what the variable/nCube describes. The element may be repeated only to support multiple language expressions of the content.",
            examples = c(
                "<var><anlysUnit>This variable reports election returns at the constituency level.</anlysUnit></var>",
                "<nCube><anlysUnit>Household</anlysUnit></nCube>"
            )
        ),
        anlyUnit = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the Analysis Unit CV item. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                unit = list(
                    type = "xs:string",
                    description = "Facilitates the development of a controlled vocabulary for this element. DEPRECATED",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sumDscr",
            children = c(),
            title = "Unit of Analysis",
            description = "Basic unit of analysis or observation that the file describes: individuals, families/households, groups, institutions/organizations, administrative units, etc. This element has been changed into a conceptualTextType supporting the use of exernal controlled vocabularies. Use the internal \"concept\" element to reference a controlled vocabulary.",
            examples = c(
                "<anlyUnit>individuals</anlyUnit>",
                "<anlyUnit><concept vocabURI=\"urn:ddi.cv:analysisunit:1.0\">Individual<anlyUnit>"
            )
        ),
        attribute = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "usage",
            children = c(),
            title = "Attribute",
            description = "Identifies an attribute within the element(s) identified by the selector or specificElements in which the controlled vocabulary is used. The fully qualified name used here must correspond to that in the instance, which is to say that if the attribute is namespace qualified, the prefix used here must match that which is defined in the instance.",
            examples = "<attribute>type</attribute>"
        ),
        AuthEnty = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the Organisation name, which may be the contents of the AuthEnty element or the @affiliation attribute of AuthEnty specifies a person. ISO 639-1 codes codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation of the authoring entity.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the authoring entity.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "rspStmt",
            children = c(),
            title = "Authoring Entity/Primary Investigator",
            description = c(
                "The person, corporate body, or agency responsible for the work's substantive and intellectual content. Repeat the element for each author, and use \"abbr\" and/or \"affiliation\" attribute if available. Invert first and last name and use commas. Author of data collection (codeBook/stdyDscr/citation/rspStmt/AuthEnty) maps to Dublin Core element \"Creator\". Inclusion of this element in codebook is recommended. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
                "The \"author\" in the Document Description should be the individual(s) or organization(s) directly responsible for the intellectual content of the DDI version, as distinct from the person(s) or organization(s) responsible for the intellectual content of the earlier paper or electronic edition from which the DDI edition may have been derived."
            ),
            examples = c(
                "<AuthEnty>United States Department of Commerce. Bureau of the Census</AuthEnty>",
                "<AuthEnty abbr=\"Insee\">National Institute of Statistics and Economic Studies</AuthEnty>",
                "<AuthEnty affiliation=\"European Commission\">Rabier, Jacques-Rene</AuthEnty>",
                "<AuthEnty personalID=\"0000-0002-4402-9644\" typeOfPersonalID=\"ORCID\">Shepherdson, John</AuthEnty>"
            )
        ),
        authorizationStatement = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "studyAuthorization",
            children = c(),
            title = "Authorization Statement",
            description = "The text of the authorization. Use XHTML to capture significant structure in the document.",
            examples = "<authorizationStatement>Required documentation covering the study purpose, disclosure information, questionnaire content, and consent statements was delivered to the OUHS on 2010-10-01 and was reviewed by the compliance officer. Statement of authorization for the described study was issued on 2010-11-04</authorizationStatement>"
        ),
        authorizingAgency = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation of the authorizing agent or agency.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation of the authorizing agent's or agency's name.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the authorizing agency.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "studyAuthorization",
            children = c(),
            title = "Authorizing Agency",
            description = "Name of the agent or agency that authorized the study. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = "<authorizingAgency affiliation=\"Purdue University\" abbr=\"OUHS\">Office for Use of Human Subjects</authorizingAgency> "
        ),
        avlStatus = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "setAvail",
            children = c(),
            title = "Availability Status",
            description = "Statement of collection availability. An archive may need to indicate that a collection is unavailable because it is embargoed for a period of time, because it has been superseded, because a new edition is imminent, etc. It is anticipated that a controlled vocabulary will be developed for this element.",
            examples = "<avlStatus>This collection is superseded by CENSUS OF POPULATION, 1880 [UNITED STATES]: PUBLIC USE SAMPLE (ICPSR 6460).</avlStatus>"
        ),
        backward = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                qstn = list(
                    type = "xs:IDREFS",
                    description = "Specifies the question ID(s), space delimited.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "qstn",
            children = c(),
            title = "Backflow",
            description = "Contains a reference to IDs of possible preceding questions.",
            examples = c(
                "<var><qstn><backward qstn=\"Q12 Q13 Q14 Q15\">For responses on a similar topic, see questions 12-15.</backward></qstn></var>",
                "<var><qstn><backward qstn=\"Q143\"/></qstn></var>"
            )
        ),
        biblCit = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                format = list(
                    type = "xs:string",
                    description = "Enables specification of the particular citation style used, e.g., APA, MLA, Chicago, etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("citation", "docSrc", "fileCitation", "sourceCitation"),
            children = c(),
            title = "Bibliographic Citation",
            description = "Complete bibliographic reference containing all of the standard elements of a citation that can be used to cite the work.",
            examples = "<biblCit format=\"MRDF\">Rabier, Jacques-Rene, and Ronald Inglehart. EURO-BAROMETER 11: YEAR OF THE CHILD IN EUROPE, APRIL 1979 [Codebook file]. Conducted by Institut Francais D'Opinion Publique (IFOP), Paris, et al. ICPSR ed. Ann Arbor, MI: Inter-university Consortium for Political and Social Resarch [producer and distributor], 1981.</biblCit>"
        ),
        boundPoly = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "sumDscr",
            children = "polygon",
            title = "Geographic Bounding Polygon",
            description = c(
                "This field allows the creation of multiple polygons to describe in a more detailed manner the geographic area covered by the dataset. It should only be used to define the outer boundaries of a covered area. For example, in the United States, such polygons can be created to define boundaries for Hawaii, Alaska, and the continental United States, but not interior boundaries for the contiguous states. This field is used to refine a coordinate-based search, not to actually map an area. ",
                "If the boundPoly element is used, then geoBndBox MUST be present, and all points enclosed by the boundPoly MUST be contained within the geoBndBox. Elements westBL, eastBL, southBL, and northBL of the geoBndBox should each be represented in at least one point of the boundPoly description. "
            ),
            examples = c(
                "<geogCover>Nevada State</geogCover>",
                "<boundPoly><polygon><point><gringLat>42.002207</gringLat><gringLon>-120.005729004</gringLon></point><point><gringLat>42.002207</gringLat><gringLon>-114.039663</gringLon></point><point><gringLat>35.9</gringLat><gringLon>-114.039663</gringLon></point><point><gringLat>36.080</gringLat><gringLon>-114.544</gringLon></point><point><gringLat>35.133</gringLat><gringLon>-114.542</gringLon></point><point><gringLat>35.00208499998</gringLat><gringLon>-114.63288</gringLon></point><point><gringLat>35.00208499998</gringLat><gringLon>-114.63323</gringLon></point><point><gringLat>38.999</gringLat><gringLon>-120.005729004</gringLon></point><point><gringLat>42.002207</gringLat><gringLon>-120.005729004</gringLon></point></polygon></boundPoly>",
                "<geogCover>Norway</geogCover>",
                "<boundPoly><polygon><point><gringLat>80.76416</gringLat><gringLon>33.637497</gringLon></point><point><gringLat>80.76416</gringLat><gringLon>10.2</gringLon></point><point><gringLat>62.48395</gringLat><gringLon>4.789583</gringLon></point><point><gringLat>57.987915</gringLat><gringLon>4.789583</gringLon></point><point><gringLat>57.987915</gringLat><gringLon>11.8</gringLon></point><point><gringLat>61.27794</gringLat><gringLon>13.2336</gringLon></point><point><gringLat>63.19012</gringLat><gringLon>13.2336</gringLon></point><point><gringLat>67.28615</gringLat><gringLon>17.24580</gringLon></point><point><gringLat>68.14297</gringLat><gringLon>21.38362</gringLon></point><point><gringLat>68.14297</gringLat><gringLon>25.50054</gringLon></point><point><gringLat>69.39685</gringLat><gringLon>27.38137</gringLon></point><point><gringLat>68.76991</gringLat><gringLon>28.84424</gringLon></point><point><gringLat>68.76991</gringLat><gringLon>31.31021</gringLon></point><point><gringLat>71.42</gringLat><gringLon>31.31021</gringLon></point><point><gringLat>71.42</gringLat><gringLon>33.637497</gringLon></point><point><gringLat>80.76416</gringLat><gringLon>33.637497</gringLon></point></polygon></boundPoly>"
            )
        ),
        caseQnty = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("dimensns", "recDimnsn"),
            children = c(),
            title = "Number of cases / Record Quantity",
            description = "Number of cases or observations.",
            examples = "<caseQnty>1011</caseQnty>"
        ),
        catgry = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                missing = list(
                    type = "xs:string",
                    description = "Indicates whether this category group contains missing data or not.",
                    values = c("Y", "N"),
                    default = "N",
                    optional = TRUE,
                    recommended = FALSE
                ),
                missType = list(
                    type = "xs:string",
                    description = "Type of missing data, e.g., inap., don't know, no answer, etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                country = list(
                    type = "xs:string",
                    description = "Allows for the denotation of country-specific category values",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Records the ID values of all elements within the summary data description that apply to this category.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements within the Data Access and etadata Access sections description that apply to this category.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                excls = list(
                    type = "xs:boolean",
                    description = "Exclusiveness, should be set to \"false\" if the category can appear in more than one place in the classification hierarchy.",
                    values = c("true", "false"),
                    default = "true",
                    optional = TRUE,
                    recommended = FALSE
                ),
                catgry = list(
                    type = "xs:IDREFS",
                    description = "References any child categories of this category element. Used to capture nested hierarchies of categories.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                level = list(
                    type = "xs:IDREF",
                    description = "Reference to the catLevel ID in which this category exists.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c("catValu", "labl", "txt", "catStat", "mrow"),
            title = "Category",
            description = "A description of a particular response.",
            examples = c(
                "<catLevel ID=\"Level1\" levelnm=\"Broader sectors\"/>",
                "<catLevel ID=\"Level2\" levelnm=\"Narrower sectors\"/>",
                "<catLevel ID=\"Level3\" levelnm=\"Occupations\"/>",
                "<catgry ID=\"C1\" catgry=\"C2\" Level=\"Level1\"><catValu>0</catValu><labl>Management, professional and related occupations</labl></catgry>",
                "<catgry ID=\"C2\" catgry=\"C3, C4\" Level=\"Level2\"><catValu>01</catValu><labl>Management occupations</labl></catgry>",
                "<catgry ID=\"C3\" Level=\"Level3\"><catValu>011</catValu><labl>Top executives</labl></catgry>",
                "<catgry ID=\"C4\" Level=\"Level3\"><catValu>012</catValu><labl>Financial managers</labl></catgry>"
            )
        ),
        catgryGrp = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                missing = list(
                    type = "xs:string",
                    description = "Indicates whether this category group contains missing data or not",
                    values = c("Y", "N"),
                    default = "N",
                    optional = TRUE,
                    recommended = FALSE
                ),
                missType = list(
                    type = "xs:string",
                    description = "Specifies the type of missing data, e.g., inap., don't know, no answer, etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                catgry = list(
                    type = "xs:IDREFS",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                catGrp = list(
                    type = "xs:IDREFS",
                    description = "Indicates all the subsidiary category groups which nest underneath the current category group. This allows for the encoding of a hierarchical structure of category groups.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                levelno = list(
                    type = "xs:string",
                    description = "Adds a level number",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                levelnm = list(
                    type = "xs:string",
                    description = "Specifies a level name to the category group",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                compl = list(
                    type = "xs:boolean",
                    description = "Completeness, should be set to \"false\" if the category group is incomplete (not a complete aggregate of all sub-nodes or chil).",
                    values = c("true", "false"),
                    default = "true",
                    optional = TRUE,
                    recommended = FALSE
                ),
                excls = list(
                    type = "xs:boolean",
                    description = "Exclusiveness, should be set to \"false\" if the category group can appear in more than one place in the classification hierarchy.",
                    values = c("true", "false"),
                    default = "true",
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c("labl", "catStat", "txt"),
            title = "Category Group",
            description = "A description of response categories that might be grouped together.",
            examples = c()
        ),
        catLevel = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                levelnm = list(
                    type = "xs:string",
                    description = "Level name",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                geoMap = list(
                    type = "xs:IDREFS",
                    description = "IDs of the appropriate geoMap elements, since a category level may be linked to one or more maps of the variable content.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c(),
            title = "Category Level",
            description = "Used to describe the levels of the category hierarchy. Note that we do not indicate nesting levels or roll-up structures here. This is done to be able to support ragged hierarchies.",
            examples = c(
                "<catLevel ID=\"Level1\" levelnm=\"Broader sectors\"/>",
                "<catLevel ID=\"Level2\" levelnm=\"Narrower sectors\"/>",
                "<catLevel ID=\"Level3\" levelnm=\"Occupations\" geoMap=\"GEO_1 GEO_2\"/>"
            )
        ),
        catStat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Indicates the type of statistics presented - frequency, percent, or crosstabulation.",
                    values = c("freq", "percent", "crosstab", "other"),
                    default = "freq",
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherType = list(
                    type = "xs:NMTOKEN",
                    description = "Specifies the other type, if \"type\" is \"other\". It should take a value from a controlled vocabulary. This option should only be used when applying a controlled vocabulary to this attribute.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "Refers to the external object containing the information.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                methrefs = list(
                    type = "xs:IDREFS",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                wgtd = list(
                    type = "xs:string",
                    description = "Type of statistic: weighted or not weighted.",
                    values = c("wgtd", "not-wgtd"),
                    default = "not-wgtd",
                    optional = TRUE,
                    recommended = FALSE
                ),
                wgt_var = list(
                    type = "xs:IDREFS",
                    description = c("A reference to the variable containing the weight used.", "(the actual attribute is named \"wgt-var\" in the final XML file.)"),
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                weight = list(
                    type = "xs:IDREFS",
                    description = "The value of a standard weight.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access and Metadata Access section that describe access conditions.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("catgry", "catgryGrp"),
            children = c(),
            title = "Category Level Statistic",
            description = c(
                "May include frequencies, percentages, or crosstabulation results. This field can contain one of the following:",
                "1. textual information (e.g., PCDATA), or ",
                "2. non-parseable character data (e.g., the statistics), or ",
                "3. some other form of external information (table, image, etc.) ",
                "In case 1, the tag can be used to mark up character data; tables can also be included in the actual markup. In cases 2 or 3, the element can be left empty and the \"URI\" attribute used to refer to the external object containing the information. "
            ),
            examples = "<catStat type=\"freq\" wgtd=\"not-wgtd\">256</catStat>"
        ),
        catValu = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "catgry",
            children = c(),
            title = "Category Value",
            description = "The explicit response option for a category.",
            examples = "<catgry missing=\"Y\" missType=\"inap\"><catValu>9</catValu></catgry>"
        ),
        citation = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                MARCURI = list(
                    type = "xs:string",
                    description = "MAchine Readable Citation URI, link to the MARC record for the citation",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("docDscr", "othRefs", "otherMat", "relMat", "relPubl", "relStdy", "stdyDscr"),
            children = c("titlStmt", "rspStmt", "prodStmt", "distStmt", "serStmt", "verStmt", "biblCit", "holdings", "notes"),
            title = "Bibliographic Citation",
            description = c(
                "This element encodes the bibliographic information for the work at the level specified: ",
                "(1) Document Description, Citation (of Marked-up Document), ",
                "(2) Document Description, Citation (of Marked-up Document Source), ",
                "(3) Study Description, Citation (of Study), ",
                "(4) Study Description, Other Material, and ",
                "(5) Other Material for the study itself. ",
                "The elements sourceCitation and fileCitation use this structure to provide citations for a source or data file respectively. Bibliographic information includes title information, statement of responsibility, production and distribution information, series and version information, text of a preferred bibliographic citation, and notes (if any). "
            ),
            examples = c()
        ),
        citReq = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "useStmt",
            children = c(),
            title = "Citation Requirement",
            description = "Text of requirement that a data collection should be cited properly in articles or other publications that are based on analysis of the data.",
            examples = "<citReq>Publications based on ICPSR data collections should acknowledge those sources by means of bibliographic citations. To ensure that such source attributions are captured for social science bibliographic utilities, citations must appear in footnotes or in the reference section of publications.</citReq>"
        ),
        cleanOps = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                agency = list(
                    type = "xs:string",
                    description = "Specifies the agency doing the data cleaning.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Cleaning Operations",
            description = "Methods used to \"clean\" the data collection, e.g., consistency checking, wild code checking, etc.",
            examples = "<cleanOps agency=\"NHGIS\">Checks for undocumented codes were performed, and data were subsequently revised in consultation with the principal investigator.</cleanOps>"
        ),
        codeBook = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                version = list(
                    type = "xs:string",
                    description = "Version number of the DDI specification.",
                    values = c("2.5", "2.6"),
                    default = "2.5",
                    optional = FALSE,
                    recommended = FALSE
                ),
                codeBookAgency = list(
                    type = "xs:NCName",
                    description = "Agency name of the creator or maintainer of the codeBook instance as a whole. This is designed to support forward compatibility with DDI-Lifecycle. Recommend the agency name as filed with the DDI Agency ID Registry with optional additional sub-agency extensions.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                'xsi:schemaLocation' = list(
                    type = "xs:string",
                    description = "URL or URN containing the location of the XML file schema.",
                    values = c(),
                    default = "ddi:codebook:2_5 codebook.xsd",
                    optional = FALSE,
                    recommended = FALSE
                ),
                'xmlns:xsi' = list(
                    type = "xs:string",
                    description = "URL or URN containing the location of the XML Schema Instance.",
                    values = c(),
                    default = "http://www.w3.org/2001/XMLSchema-instance",
                    optional = FALSE,
                    recommended = FALSE
                ),
                'xmlns:xsd' = list(
                    type = "xs:string",
                    description = "URL or URN containing the location of the XML Schema Instance.",
                    values = c(),
                    default = "http://www.w3.org/2001/XMLSchema",
                    optional = FALSE,
                    recommended = FALSE
                ),
                xmlns = list(
                    type = "xs:string",
                    description = "The DDI Codebook XML namespace.",
                    values = c(),
                    default = "ddi:codebook:2_5",
                    optional = FALSE,
                    recommended = FALSE
                )
            ),
            parents = "",
            children = c("docDscr", "stdyDscr", "fileDscr", "dataDscr", "otherMat"),
            title = "Codebook",
            description = c(
                "Every element in the DDI DTD/Schema has the following attributes:",
                "- ID - This uniquely identifies each element.",
                "- xml-lang - Use of this attribute is deprecated, and it will no longer be supported in the next major version of the DDI specification. For newly created XML documents, please use xml:lang.",
                "- xml:lang - This attribute specifies the language used in the contents and attribute values of any element in the XML document. Use of ISO (<a href=\"http://www.iso.org/\">www.iso.org</a>) language codes is recommended.",
                "- source - This attribute identifies the source that provided information in the element. If the documentation contains two differing sets of information on Sampling Procedure -- one provided by the data producer and one by the archive where the data is deposited -- this information can be distinguished through the use of the source attribute.",
                "Note also that the DDI contains a linking mechanism permitting arbitrary links between internal elements (See Link) and from internal elements to external sources (See ExtLink). Note that the use of these two elements has been DEPRECATED in version 2.6."
            ),
            examples = c()
        ),
        codeListAgencyName = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "controlledVocabUsed",
            children = c(),
            title = "Code List Agency Name",
            description = "Agency maintaining the code list. This name should be registered in the DDI Agency Registry.",
            examples = "<codeListAgencyName>DDI Alliance</codeListAgencyName>"
        ),
        codeListID = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "controlledVocabUsed",
            children = c(),
            title = "Code List ID",
            description = "Identifies the code list that the value is taken from. If this is a URN or URI place in codeListURN.",
            examples = "<codeListID>TimeMethod</codeListID>"
        ),
        codeListName = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "controlledVocabUsed",
            children = c(),
            title = "Code List Name",
            description = "Identifies the code list that the value is taken from with a human-readable name.",
            examples = "<codeListName>Time Method</codeListName>"
        ),
        codeListSchemeURN = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "controlledVocabUsed",
            children = c(),
            title = "Code List Scheme URN",
            description = "Identifies the code list scheme using a URN.",
            examples = " <codeListSchemeURN>http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.1_Genericode1.0_DDI-CVProfile1.0.xml</codeListSchemeURN> "
        ),
        codeListURN = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "controlledVocabUsed",
            children = c(),
            title = "Code List URN",
            description = "Identifies the code list that the value is taken from with a URN.",
            examples = "<codeListURN>urn:ddi-cv:TimeMethod:1.1</codeListURN>"
        ),
        codeListVersionID = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "controlledVocabUsed",
            children = c(),
            title = "Code List Version ID",
            description = "Version of the code list. (Default value is 1.0)",
            examples = "<codeListVersionID>1.1</codeListVersionID>"
        ),
        codingInstructions = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Type of (re)coding instruction. DEPRECATED",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                relatedProcesses = list(
                    type = "xs:IDREFS",
                    description = "Allows linking a coding instruction to one or more processes such as dataProcessing, dataAppr, cleanOps, etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "method",
            children = c("typeOfCodingInstruction", "txt", "command"),
            title = "Coding Instructions",
            description = "Describe specific coding instructions used in data processing, cleaning, assession, or tabulation. Use the element \"txt\" to describe instructions in a human readable form. Use the typeOfCodingInstruction to indicate the type of instruction with or without the use of a controlled vocabulary.",
            examples = "<codingInstructions relatedProcesses=\"cleanOps_7334\"><typeOfCodingInstruction>recode</typeOfCodingInstruction><txt>recode undocumented/wild codes to missing, i.e., 0.</txt><command formalLanguage=\"SPSS\">RECODE V1 TO V100 (10 THROUGH HIGH = 0)</command></codingInstructions>"
        ),
        codInstr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "var",
            children = c(),
            title = "Coder Instructions",
            description = "Any special instructions to those who converted information from one form to another for a particular variable. This might include the reordering of numeric information into another form or the conversion of textual information into numeric information.",
            examples = "<var><codInstr>Use the standard classification tables to present responses to the question: What is your occupation? into numeric codes.</codInstr></var>"
        ),
        cohort = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                catRef = list(
                    type = "xs:IDREF",
                    description = "References the ID of the actual category being used.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                value = list(
                    type = "xs:string",
                    description = "Indicates the actual value attached to the category that is being used.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dmns",
            children = "range",
            title = "Cohort",
            description = "The element cohort is used when the nCube contains a limited number of categories from a particular variable, as opposed to the full range of categories.",
            examples = "<dmns><cohort catRef=\"CV24_1\" value=\"1\"/></dmns>"
        ),
        collDate = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "ISO standard for dates (YYYY-MM-DD) is recommended for use.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                event = list(
                    type = "xs:string",
                    description = "Specify \"start\", \"end\", or \"single\" for each date entered.",
                    values = c("start", "end", "single"),
                    default = "single",
                    optional = FALSE,
                    recommended = FALSE
                ),
                cycle = list(
                    type = "xs:string",
                    description = "Sspecifies the relevant cycle, wave, or round of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sumDscr",
            children = c(),
            title = "Date of Collection",
            description = "Contains the date(s) when the data were collected. Maps to Dublin Core element \"Coverage\". It can also contain a free text expression of the collection period, in which case the attribute \"xmlang\" becomes mandatory. ISO 639-1 codes are strongly encouraged to be used.",
            examples = "<collDate event=\"single\" date=\"1998-11-10\" xmlang=\"ro\">10 noiembrie 1998</collDate>"
        ),
        collectorTraining = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Type of training being described. DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Collector Training",
            description = "Describes the training provided to data collectors including internviewer training, process testing, compliance with standards etc. This is repeatable for language and to capture different aspects of the training process. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevent concept.",
            examples = "<collectorTraining><concept vocab=\"TrainingObject\" vocabURI=\"http://xyzdatacollection.org/vocabularies/TrainingObject\" vocabURI=\"http://xyzdatacollection.org/vocabularies/TrainingObject#InterviewerTraining\">InterviewerTraining</concept>Describe research project, describe population and sample, suggest methods and language for approaching subjects, explain questions and key terms of survey instrument.</collectorTraining> "
        ),
        collMode = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c("concept", "txt"),
            title = "Mode of Data Collection",
            description = "The method used to collect the data; instrumentation characteristics. As of version 2.6 this element is a conceptualtext type and supports the use of a controlled vocabulary. XHTML formatting may be used in the txt element for forward-compatibility with DDI Lifecycle.",
            examples = c(
                "<collMode>telephone interviews</collMode>",
                "<collMode><concept vocab=\"ModeOfCollection\" vocabURI=\"urn:ddi:int.ddi.cv:ModeOfCollection:3.0\">Interview.FaceToFace</concept>face-to-face interviews</collMode>",
                "<collMode>mail questionnaires</collMode>",
                "<collMode>computer-aided telephone interviews (CATI)</collMode>"
            )
        ),
        collSitu = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c(),
            title = "Characteristics of Data Collection Situation",
            description = "Description of noteworthy aspects of the data collection situation. Includes information on factors such as cooperativeness of respondents, duration of interviews, number of call-backs, etc.",
            examples = "<collSitu>There were 1,194 respondents who answered questions in face-to-face interviews lasting approximately 75 minutes each.</collSitu>"
        ),
        collSize = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "setAvail",
            children = c(),
            title = "Extent of Collection",
            description = "Summarizes the number of physical files that exist in a collection, recording the number of files that contain data and noting whether the collection contains machine-readable documentation and/or other supplementary files and information such as data dictionaries, data definition statements, or data collection instruments. ",
            examples = "<collSize>1 data file + machine-readable documentation (PDF) + SAS data definition statements</collSize>"
        ),
        command = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                formalLanguage = list(
                    type = "xs:string",
                    description = "Identifies the language of the command code.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "codingInstructions",
            children = c(),
            title = "Command",
            description = "Provide command code for the coding instruction.",
            examples = "<command formalLanguage=\"SPSS\">RECODE V1 TO V100 (10 THROUGH HIGH = 0)</command> "
        ),
        complete = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "setAvail",
            children = c(),
            title = "Completeness of Study Stored",
            description = "This item indicates the relationship of the data collected to the amount of data coded and stored in the data collection. Information as to why certain items of collected information were not included in the data file stored by the archive should be provided. ",
            examples = "<complete>Because of embargo provisions, data values for some variables have been masked. Users should consult the data definition statements to see which variables are under embargo. A new version of the collection will be released by ICPSR after embargoes are lifted.</complete>"
        ),
        complianceDescription = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "standardsCompliance",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        concept = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("anlyUnit", "anlysUnit", "collMode", "collectorTraining", "dataAppr", "dataChck", "dataKind", "dataProcessing", "evaluationProcess", "frequenc", "geogCover", "geogUnit", "instrumentDevelopment", "nCubeGrp", "nation", "resInstru", "sampProc", "srcOrig", "stdyClas", "timeMeth", "unitType", "universe", "var", "varGrp", "weight"),
            children = c(),
            title = "Concept",
            description = "The general subject to which the parent element may be seen as pertaining. This element serves the same purpose as the keywords and topic classification elements, but at the data description level.",
            examples = c(
                "<nCubeGrp><concept>Income</concept></nCubeGrp>",
                "<nCubeGrp><concept vocab=\"LCSH\" vocabURI=\"http://lcweb.loc.gov/catdir/cpso/lcco/lcco.html\" source=\"archive\">more experience</concept></nCubeGrp>",
                "<var><concept>Income</concept></var>",
                "<var><concept vocab=\"LCSH\" vocabURI=\"http://lcweb.loc.gov/catdir/cpso/lcco/lcco.html\" vocabInstanceURI=\"http://lcweb.loc.gov/catdir/cpso/lcco#SF311-312\" source=\"archive\">SF: 311-312 draft horses</concept></var>"
            )
        ),
        conditions = list(
            optional = TRUE, # CESSDA has it mandatory...!
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                elementVersion = list(
                    type = "xs:string",
                    description = "Version of the element.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                )
            ),
            parents = "useStmt",
            children = c(),
            title = "Conditions",
            description = "Indicates any additional information that will assist the user in understanding the access and use conditions of the data collection.",
            examples = "<conditions>The data are available without restriction. Potential users of these datasets are advised, however, to contact the original principal investigator Dr. J. Smith (Institute for Social Research, The University of Michigan, Box 1248, Ann Arbor, MI 48106), about their intended uses of the data. Dr. Smith would also appreciate receiving copies of reports based on the datasets.</conditions>"
        ),
        confDec = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                required = list(
                    type = "xs:string",
                    description = "Aids machine processing of this element, default specification is \"yes\".",
                    values = c("yes", "no"),
                    default = "yes",
                    optional = TRUE,
                    recommended = FALSE
                ),
                formNo = list(
                    type = "xs:string",
                    description = "Indicates the number or ID of the form that the user must fill out.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "Provides a URN or URL for online access to a confidentiality declaration form.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "useStmt",
            children = c(),
            title = "Confidentiality Declaration",
            description = "This element is used to determine if signing of a confidentiality declaration is needed to access a resource.",
            examples = c(
                "<confDec formNo=\"1\">To download this dataset, the user must sign a declaration of confidentiality.</confDec>",
                "<confDec URI=\"http://www.icpsr.umich.edu/HMCA/CTSform/contents.html\" required=\"yes\"> To obtain this dataset, the user must complete a Restricted Data Use Agreement.</confDec>"
            )
        ),
        ConOps = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                agency = list(
                    type = "xs:string",
                    description = "Reference to the agency that performed the control operation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Control Operations",
            description = "Methods to facilitate data control performed by the primary investigator or by the data archive. Specify any special programs used for such operations.",
            examples = "<ConOps source=\"archive\" agency=\"ICPSR\">Ten percent of data entry forms were reentered to check for accuracy.</ConOps>"
        ),
        contact = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "Indicates a URN or URL for the homepage of the contact individual.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                email = list(
                    type = "xs:string",
                    description = "Indicates an email address for the contact individual.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the contact person.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("distStmt", "useStmt"),
            children = c(),
            title = "Contact Persons",
            description = "Names and addresses of individuals responsible for the work. Individuals listed as contact persons will be used as resource persons regarding problems or questions raised by the user community. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = "<contact affiliation=\"University of Wisconsin\" email=\"jsmith@wisc.edu\" URI=\"wisc.edu\" personalID=\"0000-0003-1294-0000\" typeOfPersonalID=\"orchid\">Jane Smith</contact>"
        ),
        controlledVocabUsed = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "docDscr",
            children = c("codeListID", "codeListName", "codeListAgencyName", "codeListVersionID", "codeListURN", "codeListSchemeURN", "usage"),
            title = "Controlled Vocabulary Used",
            description = "Provides information on the use of a controlled vocabulary by the DDI instance. The controlled vocabualry is identified by a codelistID, codelistName, codelistAgencyName, codelistVersionID, codelistURN, and codelistSchemeURN. The use of the controlled vocabulary is defined by the usage element.",
            examples = c()
        ),
        copyright = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "prodStmt",
            children = c(),
            title = "Copyright",
            description = "Copyright statement for the work at the appropriate level. Copyright for data collection (codeBook/stdyDscr/citation/prodStmt/copyright) maps to Dublin Core element \"Rights\". Inclusion of this element is recommended.",
            examples = "<copyright>Copyright(c) ICPSR, 2000</copyright>"
        ),
        CubeCoord = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                coordNo = list(
                    type = "xs:string",
                    description = "Coordinate number.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                coordVal = list(
                    type = "xs:string",
                    description = "Coordinate value.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                coordValRef = list(
                    type = "xs:IDREF",
                    description = "Coordinate value reference, an ID reference to the variable that carries the coordinate value.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataItem",
            children = c(),
            title = "Cube Coordinate",
            description = "This is an empty element containing only the attributes listed below. It is used to identify the coordinates of the data item within a logical nCube describing aggregate data. CubeCoord is repeated for each dimension of the nCube giving the \"coordNo\" and \"coordVal\". The attribute \"cordValRef\" is an ID reference to the variable that carries the coordinate value. The attributes provide a complete coordinate location of a cell within the nCube.",
            examples = c(
                "<CubeCoord coordNo=\"1\" coordVal=\"3\"/>",
                "<CubeCoord coordNo=\"2\" coordVal=\"7\"/>",
                "<CubeCoord coordNo=\"3\" coordVal=\"2\" coordValRef=\"AGE-3\"/>"
            )
        ),
        custodian = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "Affiliation of the custodian with an agency or organization.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation for the custodian.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the custodian.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sampleFrame",
            children = c(),
            title = "Custodian",
            description = "Custodian identifies the agency or individual who is responsible for creating or maintaining the sample frame.",
            examples = "<custodian abbr=\"DEX\">DEX Publications</custodian>"
        ),
        dataAccs = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyDscr",
            children = c("typeOfAccess", "setAvail", "useStmt", "notes"),
            title = "Data Access",
            description = "This section describes access conditions and terms of use for the data collection. In cases where access conditions differ across individual files or variables, multiple access conditions can be specified. In cases where access conditions differ across individual files, variables, or categories multiple access conditions can be specified. The access conditions applying to a study, file, variable group, variable or category can be indicated by an IDREF attribute on the study, file, variable group, nCube group, variable, category, or data item elements called \"access\".",
            examples = c()
        ),
        dataAppr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Optional typing of data appraisal processes and option for controlled vocabulary. DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "anlyInfo",
            children = c(),
            title = "Other Forms of Data Appraisal",
            description = "Other issues pertaining to data appraisal. Describe here issues such as response variance, nonresponse rate and testing for bias, interviewer and response bias, confidence levels, question bias, etc. The use of the attribute \"type\" as a means of specifiying a controlled vocabulary concept is DEPRECATED. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevant concept.",
            examples = "<dataAppr><concept vocab=\"IPUMS\">ProducerAppraised</concept>These data files were obtained from the United States House of Representatives, who received them from the Census Bureau accompanied by the following caveats: \"The numbers contained herein are not official 1990 decennial Census counts. The numbers represent estimates of the population based on a statistical adjustment method applied to the official 1990 Census figures using a sample survey intended to measure overcount or undercount in the Census results. On July 15, 1991, the Secretary of Commerce decided not to adjust the official 1990 decennial Census counts (see 56 Fed. Reg. 33582, July 22, 1991). In reaching his decision, the Secretary determined that there was not sufficient evidence that the adjustment method accurately distributed the population across and within states. The numbers contained in these tapes, which had to be produced prior to the Secretary's decision, are now known to be biased. Moreover, the tapes do not satisfy standards for the publication of Federal statistics, as established in Statistical Policy Directive No. 2, 1978, Office of Federal Statistical Policy and Standards. Accordingly, the Department of Commerce deems that these numbers cannot be used for any purpose that legally requires use of data from the decennial Census and assumes no responsibility for the accuracy of the data for any purpose whatsoever. The Department will provide no assistance in interpretation or use of these numbers.\"</dataAppr>"
        ),
        dataChck = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "fileTxt",
            children = c(),
            title = "Extent of Processing Checks",
            description = "Indicate here, at the file level, the types of checks and operations performed on the data file. Use the internal \"concept\" to make use of a controlled vocabulary The following examples, except for the last, are based on ICPSR's Extent of Processing scheme:",
            examples = c(
                "<dataChck>The archive produced a codebook for this collection.</dataChck>",
                "<dataChck>Consistency checks were performed by Data Producer/ Principal Investigator.</dataChck>",
                "<dataChck>Consistency checks performed by the archive.</dataChck>",
                "<dataChck>The archive generated SAS and/or SPSS data definition statements for this collection.</dataChck>",
                "<dataChck>Frequencies were provided by Data Producer/Principal Investigator.</dataChck>",
                "<dataChck>Frequencies provided by the archive.</dataChck>",
                "<dataChck>Missing data codes were standardized by Data Producer/ Principal Investigator.</dataChck>",
                "<dataChck>Missing data codes were standardized by the archive.</dataChck>",
                "<dataChck>The archive performed recodes and/or calculated derived variables. </dataChck>",
                "<dataChck>Data were reformatted by the archive.</dataChck>",
                "<dataChck>Checks for undocumented codes were performed by Data Producer/Principal Investigator.</dataChck>",
                "<dataChck>Checks for undocumented codes were performed by the archive.</dataChck>",
                "<dataChck><concept vocab=\"EOSDIS\" vocabURI=\"https://ghrc.nsstc.nasa.gov/uso/proc_level.html\">2</concept>Derived geophysical variables at the same resolution and location as the Level 1 source data.</dataChck>"
            )
        ),
        dataColl = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "method",
            children = c("timeMeth", "dataCollector", "collectorTraining", "frequenc", "sampProc", "sampleFrame", "targetSampleSize", "deviat", "collMode", "resInstru", "instrumentDevelopment", "sources", "collSitu", "actMin", "ConOps", "weight", "cleanOps"),
            title = "Data Collection Methodology",
            description = "Information about the methodology employed in a data collection.",
            examples = c()
        ),
        dataCollector = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                abbr = list(
                    type = "xs:string",
                    description = "Common abbreviations given to agencies.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation of the data collector.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "Role of person in the data collection process.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the data collector.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Data Collector",
            description = "The entity (individual, agency, or institution) responsible for administering the questionnaire or interview or compiling the data. This refers to the entity collecting the data, not to the entity producing the documentation.",
            examples = "<dataCollector abbr=\"SRC\" affiliation=\"University of Michigan\" role=\"questionnaire administration\">Survey Research Center</dataCollector>"
        ),
        dataDscr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "codeBook",
            children = c("varGrp", "nCubeGrp", "var", "nCube", "notes"),
            title = "Variable Description",
            description = "Description of variables.",
            examples = c()
        ),
        dataFingerprint = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Either data or dataFile.",
                    values = c("data", "dataFile"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileTxt",
            children = c("digitalFingerprintValue", "algorithmSpecification", "algorithmVersion"),
            title = "Data Fingerprint",
            description = "Allows for assigning a hash value (digital fingerprint) to the data or data file. Set the attribute flag to \"data\" when the hash value provides a digital fingerprint to the data contained in the file regardless of the storage format (ASCII, SAS, binary, etc.). One approach to compute a data fingerprint is the Universal Numerical Fingerprint (UNF). Set the attribute flag to \"dataFile\" if the digital fingerprint is only for the data file in its current storage format. Provide the digital fingerprint in digitalFingerprintValue and identify the algorithm specification used (add version as a separate entry if it is not part of the specification entry).",
            examples = "<dataFingerprint type=\"data\"><digitalFingerprintValue>UNF:3:DaYlT6QSX9r0D50ye+tXpA== </digitalFingerprintValue><algorithmSpecification>UNF v5.0 Calculation Producture [http://thedata.org/book/unf-version-5-0]</algorithmSpecification><algorithmVersion>UNF V5</algorithmVersion></dataFingerprint>  "
        ),
        dataItem = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                varRef = list(
                    type = "xs:IDREF",
                    description = "Reference to the ID of a discrete variable description.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                nCubeRef = list(
                    type = "xs:IDREF",
                    description = "Points to the appropriate nCube and the element CubeCoord to identify the coordinates of the data item within the nCube.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access and Metadata Access section that describe access conditions for this cell.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "locMap",
            children = c("CubeCoord", "physLoc"),
            title = "Data Item",
            description = "Identifies a physical storage location for an individual data entry, serving as a link between the physical location and the logical content description of each data item.",
            examples = c(
                "<dataItem varRef=\"AGE_2\" access=\"restricition_1\"><physLoc recRef=\"Rec_1\" startPos=\"5\" width=\"4\" endPos=\"8\"/></dataItem>",
                "<dataItem nCubeRef=\"AGE_SEX\" access=\"restricition_1\"><CubeCoord coordNo=\"1\" coordVal=\"3\"/><CubeCoord coordNo=\"2\" coordVal=\"1\"/><physLoc recRef=\"Rec_3\" startPos=\"5\" width=\"4\" endPos=\"8\"/></dataItem>"
            )
        ),
        dataKind = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sumDscr",
            children = c(),
            title = "Kind of Data",
            description = "The type of data included in the file: survey data, census/enumeration data, aggregate data, clinical data, event/transaction data, program source code, machine-readable text, administrative records data, experimental data, psychological test, textual data, coded textual, coded documents, time budget diaries, observation data/ratings, process-produced data, etc. This element maps to Dublin Core element \"Type\". The type attribute has been DEPRECATED. For consistant use of conceptualTextType use the included content \"concept\" and related attributes to provide a reference to a controlled vocabulary.",
            examples = "<dataKind><concept vocab=\"KindOfData\" vocabURI=\"urn:ddi:int.ddi.cv:KindOfData:1.0\">Survey</concept>survey data</dataKind>"
        ),
        dataMsng = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "fileTxt",
            children = c(),
            title = "Missing Data",
            description = "This element can be used to give general information about missing data, e.g., that missing data have been standardized across the collection, missing data are present because of merging, etc.",
            examples = c(
                "<dataMsng>Missing data are represented by blanks.</dataMsng>",
                "<dataMsng>The codes \"-1\" and \"-2\" are used to represent missing data.</dataMsng>"
            )
        ),
        dataProcessing = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "method",
            children = c(),
            title = "Data Processing",
            description = "Describes various data processing procedures not captured elsewhere in the documentation, such as topcoding, recoding, suppression, tabulation, etc. The use of the attribute \"type\" as a means of specifiying a controlled vocabulary concept is DEPRECATED. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevent concept.",
            examples = "<dataProcessing><concept>topcoding</concept>The income variables in this study (RESP_INC, HHD_INC, and SS_INC) were topcoded to protect confidentiality.</dataProcessing> "
        ),
        dataSrc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "sources",
            children = c(),
            title = "Data Sources",
            description = "Used to list the book(s), article(s), serial(s), and/or machine-readable data file(s)--if any--that served as the source(s) of the data collection. ",
            examples = c(
                "<dataSrc> \"Voting Scores.\" CONGRESSIONAL QUARTERLY ALMANAC 33 (1977), 487-498.</dataSrc>",
                "<dataSrc>United States Internal Revenue Service Quarterly Payroll File</dataSrc>"
            )
        ),
        defntn = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("nCubeGrp", "varGrp"),
            children = c(),
            title = "Definition",
            description = "Rationale for why the group was constituted in this way.",
            examples = c(
                "<varGrp><defntn>The following eight variables were only asked in Ghana.</defntn></varGrp>",
                "<nCubeGrp><defntn>The following four nCubes form a single presentation table.</defntn></nCubeGrp>"
            )
        ),
        depDate = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "ISO standard for dates (YYYY-MM-DD) is recommended.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "distStmt",
            children = c(),
            title = "Date of Deposit",
            description = "The date that the work was deposited with the archive that originally received it. The term \"archive that originally received it\" differentiates between the designated depository and any locally held copies obtained to support local use.",
            examples = "<depDate date=\"1999-01-25\">January 25, 1999</depDate>"
        ),
        depositr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation of the depositor.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the depositor.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "distStmt",
            children = c(),
            title = "Depositor",
            description = "The name of the person (or institution) who provided this work to the archive storing it. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = "<depositr abbr=\"BJS\" affiliation=\"U.S. Department of Justice\">Bureau of Justice Statistics</depositr>"
        ),
        deposReq = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "useStmt",
            children = c(),
            title = "Deposit Requirement",
            description = "Information regarding user responsibility for informing archives of their use of data through providing citations to the published work or providing copies of the manuscripts. ",
            examples = "<deposReq> To provide funding agencies with essential information about use of archival resources and to facilitate the exchange of information about ICPSR participants' research activities, users of ICPSR data are requested to send to ICPSR bibliographic citations for, or copies of, each completed manuscript or thesis abstract. Please indicate in a cover letter which data were used.</deposReq>"
        ),
        derivation = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                var = list(
                    type = "xs:IDREFS",
                    description = "ID values of the other variables in the study used to generate this derived variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c("varRange", "drvdesc", "drvcmd"),
            title = "Derivation",
            description = "Used only in the case of a derived variable, this element provides both a description of how the derivation was performed and the command used to generate the derived variable, as well as a specification of the other variables in the study used to generate the derivation. The \"var\" attribute provides the ID values of the other variables in the study used to generate this derived variable. Alternatively the variables may be expressed as a range using the varRange element. Note that use of varRange is implementation dependent. Since the order of variables can change during the execution of a script, it is computationally difficult to identify which variables belong to a variable range. It has been provided to support the automated creation of derivation information. Order should be determined by the physical order expressed in location as opposed to the order expressed in the metadata document. The sub-element \"varRange\" should only be used when the physical order of variables is available and machine-actionable.",
            examples = "<derivation var=\"V4 V9\"><drvdesc>Taxible Income (V10) expressed as a combinaton of wage and salary income (V4) plus interest income (V9)</drvdesc><drvcmd synta=\"SPSS\">V10=V4+V9</drvcmd></derivation>"
        ),
        description = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "developmentActivity",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        developmentActivity = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "studyDevelopment",
            children = c("typeOfDevelopmentActivity", "description", "participant", "resource", "outcome"),
            title = "Development Activity",
            description = "Information on the development activity including a description, set of participants, resources used, and outcomes. Use of the \"type\" attribute has been DEPRECATED. Use the element typeOfSetAvailability.",
            examples = "<developmentActivity><typeOfDevelopmentActivity vocab=\"LifecycleEventType\" vocabURI=\"https://www.ddialliance.org/Specification/DDI-CV/LifecycleEventType_1.0.html\">QuestionnaireTranslation</typeOfDevelopmentActivity><typeOfDevelopmentActivity vocab=\"DIME Questionnaire Translation\" vocabURI=\"https://dimewiki.worldbank.org/index.php?title=Questionnaire_Translation&amp;oldid=8152\">Forward Translation</typeOfDevelopentActivity><description>Translation from language A to language B of question and response text. Language experts are used. Translation is tested through round-trip translation practices. Translated question will be tested for response consistency with original language text.</description><participant affiliation=\"ISRDI\" role=\"language exert\">Ragi Yousef</participant><resource><srcCitation><titlStmt><titl>Labor Force Survey 2017-2018</titl></titlStmt><holding><URI>https://www.ilo.org/surveyLib/index.php/catalog/2549/related-materials</URI></holding></srcCitation></resource><outcome>Translated question resulted in valid replication of original language in the round trip test. Translated question resulted in statistically similar results as original language question folling testing.</outcome></developmentActivity>"
        ),
        deviat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c(),
            title = "Major Deviations from the Sample Design",
            description = "Information indicating correspondence as well as discrepancies between the sampled units (obtained) and available statistics for the population (age, sex-ratio, marital status, etc.) as a whole. XHTML formatting may be used in this element for forward-compatibility with DDI Lifecycle. ",
            examples = "<deviat>The suitability of Ohio as a research site reflected its similarity to the United States as a whole. The evidence extended by Tuchfarber (1988) shows that Ohio is representative of the United States in several ways: percent urban and rural, percent of the population that is African American, median age, per capita income, percent living below the poverty level, and unemployment rate. Although results generated from an Ohio sample are not empirically generalizable to the United States, they may be suggestive of what might be expected nationally.</deviat>"
        ),
        digitalFingerprintValue = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "dataFingerprint",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        dimensns = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "fileTxt",
            children = c("caseQnty", "varQnty", "logRecL", "recPrCas", "recNumTot"),
            title = "File Dimensions",
            description = "Dimensions of the overall file. Including the case quantity, variable quantity, logical record length, records per case, and total number of records.",
            examples = c()
        ),
        disclaimer = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "useStmt",
            children = c(),
            title = "Disclaimer",
            description = "Information regarding responsibility for uses of the data collection. This element may be repeated to support multiple language expressions of the content.",
            examples = "<disclaimer>The original collector of the data, ICPSR, and the relevant funding agency bear no responsibility for uses of this collection or for interpretations or inferences based upon such uses.</disclaimer>"
        ),
        distDate = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "The ISO standard for dates (YYYY-MM-DD) is recommended for use.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                )
            ),
            parents = "distStmt",
            children = c(),
            title = "Date of Distribution",
            description = "Date that the work was made available for distribution/presentation. If using a text entry in the element content, the element may be repeated to support multiple language expressions.",
            examples = "<distDate date=\"1999-01-25\">January 25, 1999</distDate>"
        ),
        distrbtr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the organisation name. ISO codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Institutional abbreviation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "URN or URL to the ordering service or download facility on a Web site.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the data distributor.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "distStmt",
            children = c(),
            title = "Distributor",
            description = "The organization designated by the author or producer to generate copies of the particular work including any necessary editions or revisions. Names and addresses may be specified and other archives may be co-distributors. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = c(
                "<distrbtr abbr=\"ICPSR\" affiliation=\"Institute for Social Research\" URI=\"http://www.icpsr.umich.edu\">Ann Arbor, MI: Inter-university Consortium for Political and Social Research</distrbtr>",
                "<distrbtr abbr=\"UMICH\" URI=\"https://www.umich.edu/\" personalID=\"grid.214458.e\" typeOfPersonalID=\"GRID\">University of Michigan - Ann Arbor</distrbtr>"
            )
        ),
        distStmt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("citation", "docSrc", "fileCitation", "sourceCitation"),
            children = c("distrbtr", "contact", "depositr", "depDate", "distDate"),
            title = "Distributor Statement",
            description = "Distribution statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material for study. Includes information on the distributor, contact, depositor, deposit date, and distribution date.",
            examples = c()
        ),
        dmns = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                rank = list(
                    type = "xs:string",
                    description = "Coordinate order (rank=\"1\", rank=\"2\", etc.)",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                varRef = list(
                    type = "xs:IDREF",
                    description = "Points to the variable that makes up this dimension of the nCube.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "nCube",
            children = "cohort",
            title = "Dimension",
            description = "This element defines a variable as a dimension of the nCube, and should be repeated to describe each of the cube's dimensions.",
            examples = "<dmns rank=\"3\" varRef=\"V24\"><cohort catRef=\"CV24_1\" value=\"1\"/></dmns>"
        ),
        docDscr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "codeBook",
            children = c("citation", "guide", "docStatus", "docSrc", "controlledVocabUsed", "notes"),
            title = "Document Description",
            description = "The Document Description consists of bibliographic information describing the DDI-compliant document itself as a whole. This Document Description can be considered the wrapper or header whose elements uniquely describe the full contents of the compliant DDI file. Since the Document Description section is used to identify the DDI-compliant file within an electronic resource discovery environment, this section should be as complete as possible. The author in the Document Description should be the individual(s) or organization(s) directly responsible for the intellectual content of the DDI version, as distinct from the person(s) or organization(s) responsible for the intellectual content of the earlier paper or electronic edition from which the DDI edition may have been derived. The producer in the Document Description should be the agency or person that prepared the marked-up document. Note that the Document Description section contains a Documentation Source subsection consisting of information about the source of the DDI-compliant file-- that is, the hardcopy or electronic codebook that served as the source for the marked-up codebook. These sections allow the creator of the DDI file to produce version, responsibility, and other descriptions relating to both the creation of that DDI file as a separate and reformatted version of source materials (either print or electronic) and the original source materials themselves.",
            examples = c()
        ),
        docSrc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                MARCURI = list(
                    type = "xs:string",
                    description = "MAchine Readable Citation URI, link to the MARC record for the citation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "docDscr",
            children = c("titlStmt", "rspStmt", "prodStmt", "distStmt", "serStmt", "verStmt", "biblCit", "holdings", "notes"),
            title = "Documentation Source",
            description = "Citation for the source document. This element encodes the bibliographic information describing the source codebook, including title information, statement of responsibility, production and distribution information, series and version information, text of a preferred bibliographic citation, and notes (if any). Information for this section should be taken directly from the source document whenever possible. If additional information is obtained and entered in the elements within this section, the source of this information should be noted in the source attribute of the particular element tag.",
            examples = c()
        ),
        docStatus = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "docDscr",
            children = c(),
            title = "Documentation Status",
            description = "Use this field to indicate if the documentation is being presented/distributed before it has been finalized. Some data producers and social science data archives employ data processing strategies that provide for release of data and documentation at various stages of processing. The element may be repeated to support multiple language expressions of the content.",
            examples = "<docStatus>This marked-up document includes a provisional data dictionary and brief citation only for the purpose of providing basic access to the data file. A complete codebook will be published at a later date.</docStatus>"
        ),
        drop = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "fileDerivationVars",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        drvcmd = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                syntax = list(
                    type = "xs:string",
                    description = "Indicates the command language employed (e.g., R, SPSS, SAS, Fortran, etc.).",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("derivation", "fileCommand"),
            children = c(),
            title = "Derivation Command",
            description = "The actual command used to generate the derived variable. The element may be repeated to support multiple language expressions of the content.",
            examples = "<var><derivation><drvcmd syntax=\"SPSS\">RECODE V1 TO V3 (0=1) (1=0) (2=-1) INTO DEFENSE WELFAREHEALTH. </drvcmd></derivation></var>"
        ),
        drvdesc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("derivation", "fileCommand"),
            children = c(),
            title = "Derivation Description",
            description = "A textual description of the way in which this variable was derived. The element may be repeated to support multiple language expressions of the content.",
            examples = "<var><derivation><drvdesc> VAR215.01 \"Outcome of first pregnancy\" (1988 NSFG=VAR611 PREGOUT1) If R has never been pregnant (VAR203 PREGNUM EQ 0) then OUTCOM01 is blank/inapplicable. Else, OUTCOM01 is transferred from VAR225 OUTCOME for R's 1st pregnancy. </drvdesc></derivation></var>"
        ),
        eastBL = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "geoBndBox",
            children = c(),
            title = "East Bounding Longitude",
            description = "The easternmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive east and positive north), is: -180,0 <= East Bounding Longitude Value <= 180,0",
            examples = "<eastBL>33.637497</eastBL>"
        ),
        embargo = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "ISO standard for dates (YYYY-MM-DD) is recommended for use.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                event = list(
                    type = "xs:string",
                    description = "Specifies when will the embargo end, or begin its effect.",
                    values = c("notBefore", "notAfter"),
                    default = "notBefore",
                    optional = TRUE,
                    recommended = FALSE
                ),
                format = list(
                    type = "xs:string",
                    description = "Ensures the information in this element will be machine-processable, and specifies a format for the embargo element.This attribute could be used to specify other conventions for the way that information within the embargo element is set out, if conventions for encoding embargo information were established in the future.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("nCube", "var"),
            children = c(),
            title = "Embargo",
            description = c(
                "Provides information on variables/nCubes which are not currently available because of policies established by the principal investigators and/or data producers.",
                "This element may be repeated to support multiple language expressions of the content."
            ),
            examples = "<var><embargo event=\"notBefore\" date=\"2001-09-30\"> The data associated with this variable/nCube will not become available until September 30, 2001, because of embargo provisions established by the data producers.</embargo></var>"
        ),
        EstSmpErr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "anlyInfo",
            children = c(),
            title = "Estimates of Sampling Error",
            description = "Measure of how precisely one can estimate a population value from a given sample.",
            examples = "<EstSmpErr> To assist NES analysts, the PC SUDAAN program was used to compute sampling errors for a wide-ranging example set of proportions estimated from the 1996 NES Pre-election Survey dataset. For each estimate, sampling errors were computed for the total sample and for twenty demographic and political affiliation subclasses of the 1996 NES Pre-election Survey sample. The results of these sampling error computations were then summarized and translated into the general usage sampling error table provided in Table 11. The mean value of deft, the square root of the design effect, was found to be 1.346. The design effect was primarily due to weighting effects (Kish, 1965) and did not vary significantly by subclass size. Therefore the generalized variance table is produced by multiplying the simple random sampling standard error for each proportion and sample size by the average deft for the set of sampling error computations.</EstSmpErr>"
        ),
        evaluationProcess = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "exPostEvaluation",
            children = c(),
            title = "Evaluation Process",
            description = "Describes the evaluation process followed.",
            examples = "<evaluationProcess>In-depth review of pre-collection and collection procedures</evaluationProcess>"
        ),
        evaluator = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "Affiliation of the individual or organization.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation for the individual or organization.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "The role played by the individual or organization in the evaluation process.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the evaluator.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "exPostEvaluation",
            children = c(),
            title = "Evaluator Type",
            description = "The evaluator element identifies persons or organizations involved in the evaluation. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = "<evaluator affiliation=\"United Nations\" abbr=\"UNSD\" role=\"consultant\">United Nations Statistical Division</evaluator>"
        ),
        exPostEvaluation = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                completionDate = list(
                    type = "(xs:dateTime | xs:date | xs:gYearMonth | xs:gYear)",
                    description = "Holds the date the evaluation was completed.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "stdyInfo",
            children = c("typeOfExPostEvaluation", "evaluator", "evaluationProcess", "outcomes"),
            title = "Post Evaluation Procedures",
            description = "Use this section to describe evaluation procedures not address in data evaluation processes. These may include issues such as timing of the study, sequencing issues, cost/budget issues, relevance, instituional or legal arrangments etc. of the study. The completionDate attribute holds the date the evaluation was completed. The type attribute has been DEPRECATED. Use the element typeOfExPostEvaluation to identify the type of evaluation with or without the use of a controlled vocabulary.",
            examples = "<exPostEvaluation date=\"2003\" type=\"comprehensive\"><typeOfExPostEvaluation>comprehensive</typeOfExPostEvaluation><evaluator affiliation=\"United Nations\" abbr=\"UNSD\" role=\"consultant\">United Nations Statistical Division</evaluator><evaluationProcess>In-depth review of pre-collection and collection procedures</evaluationProcess><outcomes>The following steps were highly effective in increasing response rates, and should be repeated in the next collection cycle...</outcomes></exPostEvaluation> "
        ),
        fileCitation = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                MARCURI = list(
                    type = "xs:string",
                    description = "MAchine Readable Citation URI, link to the MARC record for the citation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileTxt",
            children = c("titlStmt", "rspStmt", "prodStmt", "distStmt", "serStmt", "verStmt", "biblCit", "holdings"),
            title = "File Citation",
            description = "The complex element fileCitation provides for a full bibliographic citation option for each data file described in fileDscr. To support accurate citation of a data file the minimum element set includes: titl, IDNo, authEnty, prodStmt/producer, and prodStmt/prodDate. If a DOI is available for the data file enter this in the IDNo (this element is repeatable). If a hash value (digital fingerprint) has been created for the data file enter the information regarding its value and algorithm specification in digitalFingerprint.",
            examples = "<fileCitation><titlStmt><titl>ABC News/Washington Post Monthly Poll, December 2010</titl><IDNo>http://dx.doi.org/10.3886/ICPSR32547.v1</IDNo></titlStmt><rspStmt><AuthEnty>ABC News</AuthEnty><AuthEnty>The Washington Post</AuthEnty></rspStmt><prodStmt><producer>ABC News</producer><prodDate>2011</prodDate></prodStmt></fileCitation> "
        ),
        fileCommand = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                fileDerivationCases = list(
                    type = "xs:string",
                    description = "Add or drop cases from the referenced source files, if applicable.",
                    values = c("add", "drop"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileDerivation",
            children = c("drvdesc", "drvcmd", "fileDerivationVars"),
            title = "File Command",
            description = c(
                "The element allows for the the description of the file command by capturing a textual description of the command including the capture of pseudo code in \"drvdesc\" as well as the specific command using \"drvcmd\". Follow the same convention as the variable derivation; source=\"producer\" holds original and source=\"archive\" holds the SDTD.",
                "Provided linkage to source and target variables that were involved in this derivation command in \"fileDerivationVars\". Basically, any dropped variable from the source will only be identified and referenced here. A variable that is kept and unchanged, will have a deriviation pointing to it source, but the specific command which led to its retention will only be identified here at the file level. Variables created by a file level command will be linked here, and their derivation elements will repeat the command."
            ),
            examples = c()
        ),
        fileCont = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "fileTxt",
            children = c(),
            title = "Contents of Files",
            description = "Abstract or description of the file. A summary describing the purpose, nature, and scope of the data file, special characteristics of its contents, major subject areas covered, and what questions the PIs attempted to answer when they created the file. A listing of major variables in the file is important here. In the case of multi-file collections, this uniquely describes the contents of each file.",
            examples = "<fileCont>Part 1 contains both edited and constructed variables describing demographic and family relationships, income, disability, employment, health insurance status, and utilization data for all of 1987.</fileCont>"
        ),
        fileDerivation = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                sourceFiles = list(
                    type = "xs:IDREFS",
                    description = "Space delimited list of the \"fileTxt\" IDs used as the source(s) of the derivation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileDscr",
            children = "fileCommand",
            title = "File Derivation",
            description = "The element allows for the the description of the file commands used to creation the file deriviation.",
            examples = "<fileDerivation sourceFiles=\"F1\"><fileCommand><drvdesc>Delete AGE, MARITAL from the dataset.</drvdesc><drvcmd source=\"producer\" syntax=\"spss\">delete variables AGE MARITAL.</drvcmd><drvcmd source=\"archive\" syntax=\"sdtl-pojo\">{ \"$type\" : \"DeleteVariables\", \"command\" : \"delete\", \"sourceInformation\" : { \"lineNumberStart\" : 3, \"lineNumberEnd\" : 3, \"sourceStartIndex\" : 70, \"sourceStopIndex\" : 98, \"originalSourceText\" : \"delete variables AGE MARITAL.\" }, \"unknownProperties\" : [ ], \"canChangeData\" : false, \"variables\" : [ { \"$type\" : \"VariableSymbolExpression\", \"unknownProperties\" : [ ], \"variableName\" : \"AGE\" }, { \"$type\" : \"VariableSymbolExpression\", \"unknownProperties\" : [ ], \"variableName\" : \"MARITAL\" } ], \"$type\" : \"DeleteVariables\" }</drvcmd><fileDerivationVars><drop>AGE MARITAL</drop></fileDerivationVars></fileCommand></fileDerivation>"
        ),
        fileDerivationVars = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "fileCommand",
            children = c("keep", "drop", "add"),
            title = "File Derivation Variables",
            description = c(
                "\"keep\" - space delimited ID references to the target variables that were kept as part of this file level command.",
                "\"drop\" - space delimited ID references to the source variables that were dropped as part of this file level command",
                "\"add\" - space delimited ID references to the target variables that were created as part of this file level command"
            ),
            examples = "<keep>V1 V2 V5</keep><drop>V3</drop><add>V16</add>"
        ),
        fileDscr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                URI = list(
                    type = "xs:string",
                    description = "URN or a URL that can be used to retrieve the file.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Summary data description, references that record the ID values of all elements within the summary data description section of the Study Description that might apply to the file.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                methrefs = list(
                    type = "xs:IDREFS",
                    description = "Methodology and processing references that record the ID values of all elements within the study methodology and processing section of the Study Description that might apply to the file.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                pubrefs = list(
                    type = "xs:IDREFS",
                    description = "Provides a link to publication/citation references and records the ID values of all citations elements within Other Study Description Materials or Other Study-Related Materials that pertain to this file.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "Records the ID values of all elements in the Data Access section that describe access conditions for this file.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "codeBook",
            children = c("fileTxt", "fileDerivation", "locMap", "notes"),
            title = "Data Files Description",
            description = c(
                "Information about the data file(s) that comprises a collection. This section can be repeated for collections with multiple files. ",
                "Remarks: When a codebook documents two different physical instantiations of a data file, e.g., logical record length (or OSIRIS) and card-image version, the Data File Description should be repeated to describe the two separate files. An ID should be assigned to each file so that in the Variable section the location of each variable on the two files can be distinguished using the unique file IDs."
            ),
            examples = c(
                "<fileDscr ID=\"CARD-IMAGE\" URI=\"www.icpsr.umich.edu/cgi-bin/archive.prl?path=ICPSR&amp;num=7728\"/>",
                "<fileDscr ID=\"LRECL\" URI=\"www.icpsr.umich.edu/cgi-bin/archive.prl?path=ICPSR&amp;num=7728\"/>"
            )
        ),
        fileName = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language-specific name of file",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                )
            ),
            parents = "fileTxt",
            children = c(),
            title = "File Name",
            description = "Contains a short title that will be used to distinguish a particular file/part from other files/parts in the data collection. The element may be repeated to support multiple language expressions of the content.",
            examples = "<fileName ID=\"File1\">Second-Generation Children Data</fileName>"
        ),
        filePlac = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "fileTxt",
            children = c(),
            title = "Place of File Production",
            description = "Indicates where the file was produced. In the case of an added format version this could be the archive.",
            examples = "<filePlac>Washington, DC: United States Department of Commerce, Bureau of the Census</filePlac>"
        ),
        fileQnty = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "setAvail",
            children = c(),
            title = "Number of Files",
            description = "Total number of physical files associated with a collection.",
            examples = "<fileQnty>5 files</fileQnty>"
        ),
        fileStrc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Type of structure, predefined options.",
                    values = c("rectangular", "hierarchical", "relational", "nested", "other"),
                    default = "rectangular",
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherType = list(
                    type = "xs:NMTOKEN",
                    description = "Value specifying the other type. This attribute should only be used when applying a controlled vocabulary to this attribute.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                fileStrcRef = list(
                    type = "xs:IDREF",
                    description = "Allows for multiple data files with different coverage but the same file structure to share a single fileStrc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileTxt",
            children = c("recGrp", "notes"),
            title = "File Structure",
            description = "Type of file structure. If the file is rectangular, the next relevant element is File Dimensions. Use the complex element controlledVocabUsed to identify the controlled vocabulary to which the selected term belongs. The file structure is fully described in the first fileTxt within the fileDscr and then the fileStrc in subsequent fileTxt descriptions would reference the first fileStrcRef rather than repeat the details.",
            examples = c()
        ),
        fileTxt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "fileDscr",
            children = c("fileName", "fileCitation", "dataFingerprint", "fileCont", "fileStrc", "dimensns", "fileType", "format", "filePlac", "dataChck", "ProcStat", "dataMsng", "software", "verStmt"),
            title = "File-by-File Description",
            description = "Provides descriptive information about the data file. A file name and a full bibliographic citation for the file may be entered, as well as a data fingerprint, if available. Information about the physical properties of the data file is also supported. Make sure to fill out topcClass for the study as these can be used by the data file. Note coverage constraints in fileCont.",
            examples = c()
        ),
        fileType = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                charset = list(
                    type = "xs:string",
                    description = "Character set used in the file, e.g., US-ASCII, EBCDIC, UNICODE UTF-8, etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileTxt",
            children = c(),
            title = "Type of File",
            description = "Types of data files include raw data (ASCII, EBCDIC, etc.) and software-dependent files such as SAS datasets, SPSS export files, etc. If the data are of mixed types (e.g., ASCII and packed decimal), state that here. Note that the element varFormat permits specification of the data format at the variable level. The element may be repeated to support multiple language expressions of the content.",
            examples = "<fileType charset=\"US-ASCII\">ASCII data file</fileType>"
        ),
        format = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileTxt",
            children = c(),
            title = "Data Format",
            description = "Physical format of the data file: Logical record length format, card-image format (i.e., data with multiple records per case), delimited format, free format, etc. The element may be repeated to support multiple language expressions of the content. The use of a controlled vocabulary is supported with the use of attributes \"vocab\", \"vocabURI\", and \"vocabInstanceURI\".",
            examples = "<format vocab=\"EU Vocabularies: File Type\" vocabURI=\"http://publications.europa.eu/resource/authority/file-type\" vocabInstanceURI=\"http://publications.europa.eu/resource/authority/file-type/CSV\">CSV</format>"
        ),
        forward = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                qstn = list(
                    type = "xs:IDREFS",
                    description = "Space delimited question IDs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "qstn",
            children = c(),
            title = "Forward Progression",
            description = "Contains a reference to IDs of possible following questions.",
            examples = "<var><qstn><forward qstn=\"Q120 Q121 Q122 Q123 Q124\">If yes, please ask questions 120-124.</forward></qstn></var>"
        ),
        frameUnit = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                isPrimary = list(
                    type = "xs:boolean : true",
                    description = "Boolean, indicates whether the unit is primary or not.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sampleFrame",
            children = c("unitType", "txt"),
            title = "Frame Unit",
            description = "Provides information about the sampling frame unit.",
            examples = "<frameUnit isPrimary=\"true\"><unitType numberOfUnits=\"150000\">Primary listed owners of published phone numbers in the City of St. Paul</unitType></frameUnit> "
        ),
        frequenc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                freq = list(
                    type = "xs:string",
                    description = "DEPRECATED",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Frequency of Data Collection",
            description = "For data collected at more than one point in time, the frequency with which the data were collected. The use of the attribute \"freq\" as a means of specifiying a controlled vocabulary concept is DEPRECATED. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevent concept.",
            examples = c(
                "<freq>monthly</freq>",
                "<freq>quarterly</freq>"
            )
        ),
        fundAg = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Institutional abbreviation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "Role played, if different funding agencies sponsored different stages of the production process.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the funding agency / sponsor.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "prodStmt",
            children = c(),
            title = "Funding Agency/Sponsor",
            description = "The source(s) of funds for production of the work including abbreviation and affiliation. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = c(
                "<fundAg abbr=\"NSF\" role=\"infrastructure\">National Science Foundation</fundAg>",
                "<fundAg abbr=\"NICHD\" affiliation=\"NIH\" role=\"infrastructure\">Eunice Kennedy Shriver Institute for Child Health and Human Development</fundAg>",
                "<fundAg abbr=\"SUN\" role=\"equipment\">Sun Microsystems</fundAg>"
            )
        ),
        generalDataFormat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sumDscr",
            children = c(),
            title = "General Data Format",
            description = "Expresses the variety of data formats covered i.e. Numeric, Text, Audio, Visual, Geospatial, StillImage, Software, 3D, other. Supports the use of an external controlled vocabulary. The DDI Alliance has provided a controlled vocabulary (GeneralDataFormat) to support this element at http://www.ddialliance.org/controlled-vocabularies",
            examples = "<generalDataFormat vocab=\"GeneralDataFormat\" vocabURI=\"urn:ddi:int.ddi.cv:GeneralDataFormat:2.0\" vocabInstanceURI=\"urn:ddi:int.ddi.cv:GeneralDataFormat:2.0\">Geospatial</generalDataFormat>"
        ),
        geoBndBox = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "sumDscr",
            children = c("westBL", "eastBL", "southBL", "northBL"),
            title = "Geographic Bounding Box",
            description = "The fundamental geometric description for any dataset that models geography. GeoBndBox is the minimum box, defined by west and east longitudes and north and south latitudes, that includes the largest geographic extent of the dataset's geographic coverage. This element is used in the first pass of a coordinate-based search. If the boundPoly element is included, then the geoBndBox element MUST be included.",
            examples = c(
                "<geogCover>Nevada State</geogCover>",
                "<geoBndBox><westBL>-120.005729004</westBL><eastBL>-114.039663</eastBL><southBL>35.00208499998</southBL><northBL>42.002207</northBL></geoBndBox>",
                "<geogCover>Norway</geogCover>",
                "<geoBndBox><westBL>4.789583</westBL><eastBL>33.637497</eastBL><southBL>57.987915</southBL><northBL>80.76416</northBL></geoBndBox>"
            )
        ),
        geogCover = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "sumDscr",
            children = c(),
            title = "Geographic Coverage",
            description = "Information on the geographic coverage of the data. Includes the total geographic scope of the data, and any additional levels of geographic coding provided in the variables. Maps to Dublin Core element \"Coverage\". Inclusion of this element in the codebook is recommended. Fpor forward-compatibility, DDI Lifecycle XHTML tags may be used in this element.",
            examples = "<geogCover>State of California</geogCover>"
        ),
        geogUnit = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "sumDscr",
            children = c(),
            title = "Geographic Unit",
            description = "Lowest level of geographic aggregation covered by the data.",
            examples = "<geogUnit>state</geogUnit>"
        ),
        geoMap = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                URI = list(
                    type = "xs:string",
                    description = "URN or URL of the external map.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                mapformat = list(
                    type = "xs:string",
                    description = "Format of the map.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                levelno = list(
                    type = "xs:string",
                    description = "Level of the geographic hierarchy relayed in the map.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c(),
            title = "Geographic Map",
            description = "This element is used to point, using a \"URI\" attribute, to an external map that displays the geography in question.",
            examples = "<geoMap URI=\"https://www.dol.gov/agencies/wb/data/lfp-rate-sex-state-county\" mapormat=\"raster\" levelno=\"050\"/>"
        ),
        grantNo = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                agency = list(
                    type = "xs:string",
                    description = "Name of the agency, if more than one.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "Role in the production process, if different funding agencies sponsored different stages of the production process.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "prodStmt",
            children = c(),
            title = "Grant Number",
            description = "The grant/contract number of the project that sponsored the effort.",
            examples = "<grantNo agency=\"Bureau of Justice Statistics\">J-LEAA-018-77</grantNo>"
        ),
        gringLat = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "point",
            children = c(),
            title = "G-Ring Latitude",
            description = "Latitude (y coordinate) of a point. Valid range expressed in decimal degrees is as follows: -90,0 to 90,0 degrees (latitude)",
            examples = "<gringLat>35.00208499998</gringLat>"
        ),
        gringLon = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "point",
            children = c(),
            title = "G-Ring Longitude",
            description = "Longitude (x coordinate) of a point. Valid range expressed in decimal degrees is as follows: -180,0 to 180,0 degrees (longitude)",
            examples = "<gringLon>-114.039663</gringLon>"
        ),
        guide = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "docDscr",
            children = c(),
            title = "Guide to Codebook",
            description = "List of terms and definitions used in the documentation. Provided to assist users in using the document correctly. This element was intended to reflect the section in OSIRIS codebooks that assisted users in reading and interpreting a codebook. Each OSIRIS codebook contained a sample codebook page that defined the codebook conventions. The element may be repeated to support multiple language expressions of the content.",
            examples = c()
        ),
        holdings = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the study description page at the SP website (there can be multiple 'holding' elements for study description pages in different languages. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                location = list(
                    type = "xs:string",
                    description = "Physical location where a copy is held.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                callno = list(
                    type = "xs:string",
                    description = "The call number for a work at the location specified.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "URN or URL for accessing the electronic copy of the cited work.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                media = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("citation", "docSrc", "fileCitation", "sourceCitation"),
            children = c(),
            title = "Holdings Information",
            description = "Information concerning either the physical or electronic holdings of the cited work.",
            examples = c(
                "<holdings location=\"ICPSR DDI Repository\" callno=\"inap.\" URI=\"http://www.icpsr.umich.edu/DDIrepository/\">Marked-up Codebook for Current Population Survey, 1999: Annual Demographic File</holdings>",
                "<holdings location=\"University of Michigan Graduate Library\" callno=\"inap.\" URI=\"http://www.umich.edu/library/\">Codebook for Current Population Survey, 1999: Annual Demographic File </holdings>"
            )
        ),
        IDNo = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language used in the title. Use of ISO (www.iso.org) language codes is recommended.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                agency = list(
                    type = "xs:string",
                    description = "Name of the agency.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                isPersistentIdentifier = list(
                    type = "xs:boolean",
                    description = "Identification number is a persistent identifer.",
                    values = c("true", "false"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                level = list(
                    type = "xs:string",
                    description = "Specifies the level (study, file, or project) to which the identification number applies.",
                    values = c("study", "file", "project"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "titlStmt",
            children = c(),
            title = "Identification Number",
            description = "Unique string or number (producer's or archive's number). An \"agency\" attribute is supplied to indicate the managing agency for the identifier. Identification Number of data collection maps to Dublin Core element \"Identifier\".",
            examples = c(
                "<IDNo agency=\"ICPSR\">6678</IDNo>",
                "<IDNo agency=\"ZA\">2010</IDNo>",
                "<IDNo agency=\"DOI\" isPersistentIdentifier=\"true\" level=\"project\">10.18128/D010.V7.0</IDNo>"
            )
        ),
        imputation = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("nCube", "var"),
            children = c(),
            title = "Imputation",
            description = "According to the Statistical Terminology glossary maintained by the National Science Foundation, this is \"the process by which one estimates missing values for items that a survey respondent failed to provide,\" and if applicable in this context, it refers to the type of procedure used. When applied to an nCube, imputation takes into consideration all of the dimensions that are part of that nCube. This element may be repeated to support multiple language expressions of the content.",
            examples = "<var><imputation>This variable contains values that were derived by substitution.</imputation></var>"
        ),
        instrumentDevelopment = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Instrument Development",
            description = "Describe any development work on the data collection instrument. The use of the attribute \"type\" as a means of specifiying a controlled vocabulary concept is DEPRECATED. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevent concept.",
            examples = "<instrumentDevelopment><concept vocab=\"123surveys\" vocabURI=\"http://123surveys.com/internal/developmentProtocal\" vocabInstanceURI=\"http://123surveys.com/internal/developmentProtocal#Pretest.SplitPanel\">Pretest.SplitPanel</concept>The questionnaire was pre-tested with split-panel tests, as well as an analysis of non-response rates for individual items, and response distributions.</instrumentDevelopment>"
        ),
        invalrng = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access and Metadata Access section that describe access conditions for this range.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c("item", "range", "key", "notes"),
            title = "Range of Invalid Data Values",
            description = "Values for a particular variable that represent missing data, not applicable responses, etc.",
            examples = "<invalrng access=\"DA_4\"><range UNITS=\"INT\" min=\"98\" max=\"99\"/><key> 98 DK 99 Inappropriate </key></invalrng>"
        ),
        item = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                UNITS = list(
                    type = "xs:string",
                    description = "Specifies if numbers are integer or real.",
                    values = c("INT", "REAL"),
                    default = "INT",
                    optional = TRUE,
                    recommended = FALSE
                ),
                VALUE = list(
                    type = "xs:string",
                    description = "Specifies the actual value.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("invalrng", "valrng"),
            children = c(),
            title = "Value Item",
            description = "The counterpart to Range; used to encode individual values when the values cannot be expressed as a min/max range or when specification of the values is desired. This is an empty element consisting only of its attributes.",
            examples = c(
                "<valrng><item UNITS=\"INT\" VALUE=\"10\"/><item UNITS=\"INT\" VALUE=\"15\"/><item UNITS=\"INT\" VALUE=\"22\"/></valrng>",
                "<valrng><item VALUE=\"1\"/><item VALUE=\"2\"/><item VALUE=\"3\"/></valrng>"
            )
        ),
        ivuInstr = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "qstn",
            children = c(),
            title = "Interviewer Instructions",
            description = "Specific instructions to the individual conducting an interview.",
            examples = "<var><qstn><ivuInstr>Please prompt the respondent if they are reticent to answer this question.</ivuInstr></qstn></var>"
        ),
        keep = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "fileDerivationVars",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        key = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("invalrng", "valrng"),
            children = c(),
            title = "Range Key",
            description = "This element permits a listing of the category values and labels. While this information is coded separately in the Category element, there may be some value in having this information in proximity to the range of valid and invalid values. A table is permissible in this element.",
            examples = c(
                "<valrng><range UNITS=\"INT\" min=\"05\" max=\"80\"/><key> 05 (PSU) Parti Socialiste Unifie et extreme gauche (Lutte Ouvriere) [United Socialists and extreme left (Workers Struggle)] 50 Les Verts [Green Party] 80 (FN) Front National et extreme droite [National Front and extreme right] </key></valrng>",
                "<valrng><range UNITS=\"REAL\" minExlusive=\"0\" maxExclusive=\"10\"/><key>Responses fall between 0 and 10 but may not include those two values.</key></valrng>"
            )
        ),
        keyword = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the keyword. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "subject",
            children = c(),
            title = "Keywords",
            description = "Words or phrases that describe salient aspects of a data collection's content. Can be used for building keyword indexes and for classification and retrieval purposes. A controlled vocabulary can be employed. Maps to Dublin Core element \"Subject\".",
            examples = c(
                "<keyword vocab=\"ICPSR Subject Thesaurus\" vocabURI=\"http://www.icpsr.umich.edu/thesaurus/subject.html\" vocabInstanceURI=\"http://www.icpsr.umich.edu/thesaurus/subject#qualityOfLife\">quality of life</keyword>",
                "<keyword vocab=\"ICPSR Subject Thesaurus\" vocabURI=\"http://www.icpsr.umich.edu/thesaurus/subject.html\" vocabInstanceURI=\"http://www.icpsr.umich.edu/thesaurus/subject#family\">family</keyword>",
                "<keyword vocab=\"ICPSR Subject Thesaurus\" vocabURI=\"http://www.icpsr.umich.edu/thesaurus/subject.html\" vocabInstanceURI=\"http://www.icpsr.umich.edu/thesaurus/subject#careerGoals\">career goals</keyword>"
            )
        ),
        labl = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                level = list(
                    type = "xs:string",
                    description = "Coding of the level to which the label applies, i.e. record group, variable group, variable, category group, category, nCube group, nCube, or other study-related materials.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vendor = list(
                    type = "xs:string",
                    description = "Specifies the different labels for use with different vendors' software.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                country = list(
                    type = "xs:string",
                    description = "Denotation of country-specific labels.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements within the Summary Data Description section of the Study Description that might apply to the label.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("catgry", "catgryGrp", "nCube", "nCubeGrp", "otherMat", "recGrp", "sampleFrame", "var", "varGrp"),
            children = c(),
            title = "Label",
            description = "A short description of the parent element. In the variable label, the length of this phrase may depend on the statistical analysis system used (e.g., some versions of SAS permit 40-character labels, while some versions of SPSS permit 120 characters), although the DDI itself imposes no restrictions on the number of characters allowed. These elements include: time period covered, date of collection, nation or country, geographic coverage, geographic unit, unit of analysis, universe, and kind of data.",
            examples = c(
                "<labl level=\"catgry\" country=\"US\">Pharmacist</labl>",
                "<labl level=\"catgry\" country=\"CA\">Chemist</labl>",
                "<labl level=\"nCube\" vendor=\"SAS\">Employment. race, age, sex, age 16-64</labl>",
                "<labl level=\"nCube\" vendor=\"SPSS\">Employment status by race by age by gender for persons ages 16 to 64 years of age</labl>"
            )
        ),
        language = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                typeOfLanguageCode = list(
                    type = "xs:string",
                    description = "Type of language code used.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                languageCode = list(
                    type = "xs:string",
                    description = "Code value.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "prodStmt",
            children = c(),
            title = "Language",
            description = "A brief textual identification of the language of the cited object. DDI strongly recommend the use of language codes supported by xs:language which include the 2 and 3 character and extended structures defined by RFC5646 or its successors. Repeat for multiple languages.",
            examples = c(
                "<language typeOfLanguageCode=\"RFC5646\" languageCode=\"en-CA\">English as used in Canada</language>",
                "<language typeOfLanguageCode=\"RFC5646\" languageCode=\"fr-CA\">French as used in Canada</language>"
            )
        ),
        license = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                URI = list(
                    type = "xs:string",
                    description = "URN or URL to the legal document.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                type = list(
                    type = "xs:string",
                    description = "Licensing target.",
                    values = c("data", "metadata"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "prodStmt",
            children = c(),
            title = "License",
            description = "A legal document giving official permission to something with the resource. Recommendation is to provide the license document URI. Equates to https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/license/",
            examples = "<license type=\"metadata\" URI=\"https://creativecommons.org/licenses/by/4.0/legalcode\">CC by 4.0</license>"
        ),
        location = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                StartPos = list(
                    type = "xs:string",
                    description = "Starting position of variable",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                EndPos = list(
                    type = "xs:string",
                    description = "End position of variable",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                width = list(
                    type = "xs:string",
                    description = "Number of columns the variable occupies",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                RecSegNo = list(
                    type = "xs:string",
                    description = "Segment number, deck or card number the variable is located on",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                fileid = list(
                    type = "xs:IDREF",
                    description = "ID reference to the fileDscr element for the file that this location is within (this is necessary in cases where the same variable may be coded in two different files, e.g., a logical record length type file and a card image type file).",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                locMap = list(
                    type = "xs:IDREF",
                    description = "ID reference to the element locMap and serves as a pointer to indicate that the location information for the nCube's cells (aggregate data) is located in that section.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("nCube", "var"),
            children = c(),
            title = "Location",
            description = "This is an empty element containing only if its attributes.",
            examples = c(
                "<var><location StartPos=\"55\" EndPos=\"57\" width=\"3\" RecSegNo=\"2\" fileid=\"CARD-IMAGE\"/><location StartPos=\"167\" EndPos=\"169\" fileid=\"LRECL\"/></var>",
                "<nCube><location locMap=\"LM\"/></nCube>"
            )
        ),
        locMap = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "fileDscr",
            children = "dataItem",
            title = "Location Map",
            description = "This element maps individual data entries to one or more physical storage locations. It is used to describe the physical location of aggregate/tabular data in cases where the nCube model is employed. May also be used for var location description and is useful when the physical location of a key variable is located in different positions in different record types or when the file contains a mixture of variable and nCube description. Always use the ID attribute to support the reference from the var or nCube.",
            examples = "<locMap ID=\"LM_1\"><dataItem>...</dataItem><dataItem>...</dataItem></locMap>"
        ),
        logRecL = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("dimensns", "recDimnsn"),
            children = c(),
            title = "Logical Record Length",
            description = "Logical record length, i.e., number of characters of data in the record.",
            examples = "<logRecL>27</logRecL>"
        ),
        measure = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                varRef = list(
                    type = "xs:IDREF",
                    description = "ID reference",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                aggrMeth = list(
                    type = "xs:string",
                    description = "Indicates the type of aggregation method used.",
                    values = c("sum", "average", "count", "mode", "median", "maximum", "minimum", "percent", "other"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherAggrMeth = list(
                    type = "xs:NMTOKEN",
                    description = "Other aggregation method from a controlled vocabulary. The complex element controlledVocabUsed should be used to specify the controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                measUnit = list(
                    type = "xs:string",
                    description = "Measurement unit, for example 'km', 'miles', etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                scale = list(
                    type = "xs:string",
                    description = "Unit of scale, for example 'x1', 'x1000'.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                origin = list(
                    type = "xs:string",
                    description = "Point of origin for anchored scales.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                additivity = list(
                    type = "xs:string",
                    description = "Indicates whether an aggregate is a stock (like the population at a given point in time) or a flow (like the number of births or deaths over a certain period of time). The non-additive flag is to be used for measures that for logical reasons cannot be aggregated to a higher level - for instance, data that only make sense at a certain level of aggregation, like a classification.",
                    values = c("stock", "flow", "non-additive"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "nCube",
            children = c(),
            title = "Measure",
            description = "The element measure indicates the measurement features of the cell content: type of aggregation used, measurement unit, and measurement scale. An origin point is recorded for anchored scales, to be used in determining relative movement along the scale. Two nCubes may be identical except for their measure - for example, a count of persons by age and percent of persons by age. Measure is an empty element that only includes its attributes.",
            examples = "<measure source=\"producer\" measUnit=\"Persons\" varRef=\"V_PER\" scale=\"x1\" additivity=\"stock\" aggrMeth=\"count\"/>"
        ),
        metadataAccs = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyDscr",
            children = c("typeOfAccess", "useStmt", "notes"),
            title = "Metadata Access",
            description = "This section describes access conditions and terms of use for the metadata. In cases where access conditions differ across individual files, variables, or categories multiple access conditions can be specified. The access conditions applying to a study, file, variable group, variable or category can be indicated by an IDREF attribute on the study, file, variable group, nCube group, variable, category, or data item elements called \"access\".",
            examples = c()
        ),
        method = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyDscr",
            children = c("dataColl", "notes", "anlyInfo", "stdyClas", "dataProcessing", "codingInstructions"),
            title = "Methodology and Processing",
            description = "This section describes the methodology and processing involved in a data collection. This includes use of methods such as survey, experiment, secondary analysis, field research, sampling methods, etc. Processing covers the data collection processes, aggregation, imputation, and other post-collection data processing.",
            examples = c()
        ),
        mi = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                varRef = list(
                    type = "xs:IDREF",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "mrow",
            children = c(),
            title = "Mathematical Identifier",
            description = "Token element containing the smallest unit in the mrow that carries meaning expressed by the attribute varRef. This is an element taked from MathML.",
            examples = "<mi varRef=\"STATE\"/>"
        ),
        mrow = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "catgry",
            children = "mi",
            title = "Mathematical Row",
            description = "This element is a wrapper containing the presentation expression mi. It creates a single string without spaces consisting of the individual elements described within it. It can be used to create a single variable by concatenating other variables into a single string. It is used to create linking variables composed of multiple non-contiguous parts, or to define unique strings for various category values of a single variable. This is an element taked from MathML. The example shows the use of mrow in the context of a code based on two separate variables.",
            examples = "<catgry><labl>Unique county code</labl><txt>Complete county code including the content of the variables STATE (2ch) and COUNTY (3ch) creating a unique identifying code for an individual county</txt><mrow><mi varRef=\"STATE\"/><mi varRef=\"CNTY\"/></mrow></catgy>"
        ),
        nation = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the country label. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Common abbreviations; use of ISO country codes is recommended.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                )
            ),
            parents = "sumDscr",
            children = c(),
            title = "Country",
            description = "Indicates the country or countries covered in the file. Use vocab, vocabURI, and vocabInstanceURI to identify the use of a controlled vocabulary. Maps to Dublin Core element \"Coverage\". Inclusion of this element is recommended. For forward-compatibility, DDI Lifecycle XHTML tags may be used in this element.",
            examples = c(
                "<nation vocab=\"ISO 3166-1 alpha-2\" vocabURI=\"http://www.iso.org/ISO_3166-1/alpha-2\" vocabInstanceURI=\"http://www.iso.org/ISO_3166-1/alpha-2#FI\" abbr=\"FI\" xml:lang=\"en\">Finland</nation>",
                "<nation vocab=\"ISO 3166-1 alpha-2\" vocabURI=\"http://www.iso.org/ISO_3166-1/alpha-2\" vocabInstanceURI=\"http://www.iso.org/ISO_3166-1/alpha-2#FI\" abbr=\"FI\" xml:lang=\"fi\">Suomi</nation>"
            )
        ),
        nCube = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                name = list(
                    type = "xs:string",
                    description = "Short label for the nCube. Following the rules of many statistical analysis systems such as SAS and SPSS, names are usually up to eight characters long.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Summary data description references which record the ID values of all elements within the summary data description section of the Study Description which might apply to the nCube. These elements include: time period covered, date of collection, nation or country, geographic coverage, geographic unit, unit of analysis, universe, and kind of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                methrefs = list(
                    type = "xs:IDREFS",
                    description = "Methodology and processing references which record the ID values of all elements within the study methodology and processing section of the Study Description which might apply to the nCube. These elements include information on data collection and data appraisal (e.g., sampling, sources, weighting, data cleaning, response rates, and sampling error estimates).",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                pubrefs = list(
                    type = "xs:IDREFS",
                    description = "Link to publication/citation references and records the ID values of all citations elements in Other Study Description Materials or Other Study-Related Materials that pertain to this nCube.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access section that describe access conditions for this nCube.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                dmnsQnty = list(
                    type = "xs:string",
                    description = "Number of dimensions in the nCube.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                cellQnty = list(
                    type = "xs:string",
                    description = "Total number of cells in the nCube.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataDscr",
            children = c("location", "labl", "txt", "universe", "imputation", "security", "embargo", "respUnit", "anlysUnit", "verStmt", "purpose", "dmns", "measure", "notes"),
            title = "nCube",
            description = "Describes the logical structure of an n-dimensional array, in which each coordinate intersects with every other dimension at a single point. The nCube has been designed for use in the markup of aggregate data. Repetition of the following elements is provided to support multi-language content: anlysUnit, embargo, imputation, purpose, respUnit, and security.",
            examples = c()
        ),
        nCubeGrp = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = c("General type of grouping of the nCubes. Specific nCube Groups, included within the 'type' attribute, are:", "| display: nCubes that are part of the same presentation table.", "| subject: nCubes that address a common topic or subject, e.g., income, poverty, children.", "| iteration: nCubes that appear in different sections of the data file measuring a common subject in different ways, e.g., using different universes, units of measurement, etc.", "| pragmatic: An nCube group without shared properties.", "| record: nCubes from a single record in a hierarchical file.", "| file: nCube from a single file in a multifile study.", "| other: nCubes that do not fit easily into any of the categories listed above, e.g., a group of nCubes whose documentation is in another language. A term from a controlled vocabulary may be placed into the otherType attribute if this value is used."),
                    values = c("section", "multipleResp", "grid", "display", "repetition", "subject", "version", "iteration", "analysis", "pragmatic", "record", "file", "randomized", "other"),
                    default = "other",
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherType = list(
                    type = "xs:NMTOKEN",
                    description = "Should only be used when applying a controlled vocabulary, and when the type attribute has been given a value of \"other\". Use the complex element controlledVocabUsed to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                nCube = list(
                    type = "xs:IDREFS",
                    description = "References all the IDs of the nCubes belonging to the group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                nCubeGrp = list(
                    type = "xs:IDREFS",
                    description = "References all the subsidiary nCube groups which nest underneath the current nCubeGrp. This allows for encoding of a hierarchical structure of nCube groups.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                name = list(
                    type = "xs:string",
                    description = "A name, or short label, for the nCube group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Summary data description references that record the ID values of all elements within the summary data description section of the Study Description that might apply to the group. These elements include: time period covered, date of collection, nation or country, geographic coverage, geographic unit, unit of analysis, universe, and kind of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                methrefs = list(
                    type = "xs:IDREFS",
                    description = "Methodology and processing, references which record the ID values of all elements within the study methodology and processing section of the Study Description which might apply to the group. These elements include information on data collection and data appraisal (e.g., sampling, sources, weighting, data cleaning, response rates, and sampling error estimates).",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                pubrefs = list(
                    type = "xs:IDREFS",
                    description = "Link to publication/citation references and records the ID values of all citations elements within Section codeBook/stdyDscr/othrStdyMat or codeBook/otherMat that pertain to this nCube group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in codeBook/stdyDscr/dataAccs of the document that describe access conditions for this nCube group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataDscr",
            children = c("labl", "txt", "concept", "defntn", "universe", "notes"),
            title = "nCube Group",
            description = c(
                "A group of nCubes that may share a common subject, arise from the interpretation of a single question, or are linked by some other factor. This element makes it possible to identify all nCubes derived from a simple presentation table, and to provide the original table title and universe, as well as reference the source. Specific nesting patterns can be described using the attribute nCubeGrp.",
                "nCube groups are also created this way in order to permit nCubes to belong to multiple groups, including multiple subject groups, without causing overlapping groups. nCubes that are linked by the same use of the same variable need not be identified by an nCubeGrp element because they are already linked by a common variable element. Note that as a result of the strict sequencing required by XML, all nCube Groups must be marked up before the Variable element is opened. That is, the mark-up author cannot mark up a nCube Group, then mark up its constituent nCubes, then mark up another nCube Group."
            ),
            examples = c()
        ),
        northBL = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "geoBndBox",
            children = c(),
            title = "North Bounding Latitude",
            description = "The northernmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive east and positive north), is: -90,0 <= North Bounding Latitude Value <= 90,0 ; North Bounding Latitude Value = South Bounding Latitude Value",
            examples = "<northBL>80.76416</northBL>"
        ),
        notes = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Note type.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                subject = list(
                    type = "xs:string",
                    description = "Note subject",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                level = list(
                    type = "xs:string",
                    description = "DDI level to which the note applies (study, file, variable, etc.)",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                resp = list(
                    type = "xs:string",
                    description = "Author responsible with the note.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                parent = list(
                    type = "xs:IDREFS",
                    description = "ID(s) of the element this note is related to.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sameNote = list(
                    type = "xs:IDREF",
                    description = "If the same note is used multiple times all the parent IDs can be captured in a single note and all duplicate notes can reference the note containing the related to references.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("citation", "dataAccs", "dataDscr", "docDscr", "docSrc", "fileCitation", "fileDscr", "fileStrc", "invalrng", "metadataAccs", "method", "nCube", "nCubeGrp", "otherMat", "setAvail", "sourceCitation", "stdyDscr", "stdyInfo", "valrng", "var", "varGrp", "verStmt"),
            children = c(),
            title = "Notes and comments",
            description = c(
                "For clarifying information/annotation regarding the parent element.",
                "The parent attribute is used to support capturing information obtained while preparing files for translation to DDI Lifecycle. It provides the ID(s) of the element this note is related to."
            ),
            examples = c(
                "<docDscr><verStmt><notes resp=\"Jane Smith\">Additional information on derived variables has been added to this marked-up version of the documentation.</notes></verStmt></docDscr>",
                "<docDscr><citation><notes resp=\"Jane Smith\">This citation was prepared by the archive based on information received from the markup authors.</notes></citation></docDscr>",
                "<docSrc><verStmt><notes resp=\"Jane Smith\">The source codebook was produced from original hardcopy materials using Optical Character Recognition (OCR).</notes></verStmt></docSrc>",
                "<docSrc><notes>A machine-readable version of the source codebook was supplied by the Zentralarchiv</notes></docSrc>",
                "<docDscr><notes>This Document Description, or header information, can be used within an electronic resource discovery environment.</notes></docDscr>",
                "<stdyDscr><verStmt><notes resp=\"Jane Smith\">Data for 1998 have been added to this version of the data collection.</notes></verStmt></stdyDscr>",
                "<stdyDscr><citation><notes resp=\"Jane Smith\">This citation was sent to ICPSR by the agency depositing the data.</notes></citation></stdyDscr>",
                "<stdyInfo><notes>Data on employment and income refer to the preceding year, although demographic data refer to the time of the survey.</notes></stdyInfo>",
                "<method><notes>Undocumented codes were found in this data collection. Missing data are represented by blanks.</notes></method>",
                "<method><notes>For this collection, which focuses on employment, unemployment, and gender equality, data from EUROBAROMETER 44.3: HEALTH CARE ISSUES AND PUBLIC SECURITY, FEBRUARY-APRIL 1996 (ICPSR 6752) were merged with an oversample.</notes></method>",
                "<setAvail><notes> Data from the Bureau of Labor Statistics used in the analyses for the final report are not provided as part of this collection.</notes></setAvail>",
                "<dataAccs><notes>Users should note that this is a beta version of the data. The investigators therefore request that users who encounter any problems with the dataset contact them at the above address.</notes></dataAccs>",
                "<fileStrc><notes>The number of arrest records for an individual is dependent on the number of arrests an offender had.</notes></fileStrc>",
                "<fileTxt><verStmt><notes>Data for all previously-embargoed variables are now available in this version of the file.</notes></verStmt></fileTxt>",
                "<fileDscr><notes>There is a restricted version of this file containing confidential information, access to which is controlled by the principal investigator.</notes></fileDscr>",
                "<varGrp><notes>This variable group was created for the purpose of combining all derived variables.</notes></varGrp>",
                "<varGrp><notes source=\"archive\" resp=\"John Data\">This variable group and all other variable groups in this data file were organized according to a schema developed by the adhoc advisory committee. </notes></varGrp>",
                "<nCubeGrp><notes>This nCube Group was created for the purpose of presenting a cross-tabulation between variables \"Tenure\" and \"Age of householder.\"</notes></nCubeGrp>",
                "<valrng><notes subject=\"political party\">Starting with Euro-Barometer 2 the coding of this variable has been standardized following an approximate ordering of each country's political parties along a \"left\" to \"right\" continuum in the first digit of the codes. Parties coded 01-39 are generally considered on the \"left\", those coded 40-49 in the \"center\", and those coded 60-89 on the \"right\" of the political spectrum. Parties coded 50-59 cannot be readily located in the traditional meaning of \"left\" and \"right\". The second digit of the codes is not significant to the \"left-right\" ordering. Codes 90-99 contain the response \"other party\" and various missing data responses. Users may modify these codings or part of these codings in order to suit their specific needs. </notes></valrng>",
                "<invalrng><notes>Codes 90-99 contain the response \"other party\" and various missing data responses. </notes></invalrng>",
                "<var><verStmt><notes>The labels for categories 01 and 02 for this variable, were inadvertently switched in the first version of this variable and have now been corrected.</notes></verStmt></var>",
                "<var><notes>This variable was created by recoding location of residence to Census regions.</notes></var>",
                "<nCube><verStmt><notes>The labels for categories 01 and 02 in dimension 1 were inadvertently switched in the first version of the cube, and have now been corrected.</notes></verStmt></nCube>",
                "<nCube><notes>This nCube was created to meet the needs of local low income programs in determining eligibility for federal funds.</notes></nCube>",
                "<dataDscr><notes>The variables in this study are identical to earlier waves. </notes></dataDscr>",
                "<otherMat><notes>Users should be aware that this questionnaire was modified during the CAI process.</notes></otherMat>"
            )
        ),
        origArch = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation of the original archive.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "URN or URL to the original archive.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the original archive.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "setAvail",
            children = c(),
            title = "Archive Where Study Originally Stored",
            description = "Archive from which the data collection was obtained; the originating archive. May provide \"abbr\", \"affiliation\", \"URI\", and \"personalID\" to aid in identification. If \"personalID\" is used \"typeOfPersonalID\" should be used.",
            examples = "<origArch abbr=\"GESIS\" URI=\"gesis.org\" personalID=\"018afyw53\" typeOfPersonalID=\"ROR\">GESIS Leibniz-Institut fur Sozialwissenschaften/origArch>"
        ),
        otherMat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                level = list(
                    type = "xs:NMTOKEN",
                    description = "Relationship of the other materials to components of the study. Suggested values for level include specifications of the item level to which the element applies: e.g., level=\"data\"; level=\"datafile\"; level=\"studydsc\"; level=\"study\".",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "URN or URL to the location of the other study-related materials. It needs not be used in every case; it is intended for capturing references to other materials separate from the codebook itself.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("codeBook", "otherMat"),
            children = c("typeOfSetAvailability", "labl", "txt", "notes", "table", "citation", "otherMat"),
            title = "Other Study-Related Materials",
            description = c(
                "This section allows for the inclusion of other materials that are related to the study as identified and labeled by the DTD/Schema users (encoders). The' materials may be entered as PCDATA (ASCII text) directly into the document (through use of the \"txt\" element). This ection may also serve as a \"container\" for other electronic materials such as setup files by providing a brief description of the study-related materials accompanied by the attributes \"type\" and \"level\" defining the material further. Note that the use of the attribute \"type\" has been DEPRECATED and the element \"typeOfOtherMaterial\" should be used instead. This element provide support for the use of a controlled vocabulary.",
                "Other Study-Related Materials may include: questionnaires, coding notes, R/SPSS/SAS/Stata setup files (and others), user manuals, continuity guides, sample computer software programs, glossaries of terms, interviewer/project instructions, maps, database schema, data dictionaries, show cards, coding information, interview schedules, missing values information, frequency files, variable maps, etc.",
                "In Section 5, Other Material is recursively defined."
            ),
            examples = c()
        ),
        otherQualityStatement = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "qualityStatement",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        othId = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "Role of the person / agency responsible with editing of the marked up documentation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation for the person / agency name.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the person / agency.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "rspStmt",
            children = c(),
            title = "Other Identifications /Acknowledgments",
            description = "Statements of responsibility not recorded in the title and statement of responsibility areas (collaborators). Indicate here the persons or bodies connected with the work, or significant persons or bodies connected with previous editions and not already named in the description. For example, the name of the person who edited the marked-up documentation might be cited in codeBook/docDscr/rspStmt/othId, using the \"role\" and \"affiliation\" attributes. Other identifications/acknowledgments for data collection (codeBook/stdyDscr/citation/rspStmt/othId) maps to Dublin Core element \"Contributor\". If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = "<othId role=\"editor\" affiliation=\"INRA\" personalID=\"0000-0003-1294-0000\" typeOfPersonalID=\"orchid\">Jane Smith</othId>"
        ),
        othRefs = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "othrStdyMat",
            children = "citation",
            title = "Other References Notes",
            description = "Indicates other pertinent references. Can take the form of bibliographic citations.",
            examples = c(
                "<othRefs>Part II of the documentation, the Field Representative's Manual, is provided in hardcopy form only.</othRefs>",
                "<othRefs><citation><titlStmt><titl>Work flows - Data Discovery and Dissemination: User Perspective</titl><IDNo agency=\"DOI\">10.3886/DDIBestPractices02</IDNo></titlStmt><biblCit>Work flows - Data Discovery and Dissemination: User Perspective. Dinkelmann, Karl, Michelle Edwards, Jane Fry, Chuck Humphrey, Ron Nakao, and Wendy Thomas.</biblCit></citation></othRefs>"
            )
        ),
        othrStdyMat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyDscr",
            children = c("relMat", "relStdy", "relPubl", "othRefs"),
            title = "Other Study Description Materials",
            description = "Other materials relating to the study description. This section describes other materials that are related to the study description that are primarily descriptions of the content and use of the study, such as appendices, sampling information, weighting details, methodological and technical details, publications based upon the study content, related studies or collections of studies, etc. This section may point to other materials related to the description of the study through use of the generic citation element, which is available for each element in this section. This maps to Dublin Core element \"Relation\". Note that codeBook/otherMat (Other Study-Related Materials), should be used for materials used in the production of the study or useful in the analysis of the study. The materials in codeBook/otherMat may be entered as PCDATA (ASCII text) directly into the document (through use of the txt element). That section may also serve as a \"container\" for other electronic materials by providing a brief description of the study-related materials accompanied by the \"type\" and \"level\" attributes further defining the materials. Other Study-Related Materials in codeBook/otherMat may include: questionnaires, coding notes, SPSS/SAS/Stata setup files (and others), user manuals, continuity guides, sample computer software programs, glossaries of terms, interviewer/project instructions, maps, database schema, data dictionaries, show cards, coding information, interview schedules, missing values information, frequency files, variable maps, etc.",
            examples = c()
        ),
        outcome = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "developmentActivity",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        outcomes = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "exPostEvaluation",
            children = c(),
            title = "Evaluation Outcomes",
            description = "Describe the outcomes of the evaluation.",
            examples = "<outcomes>The following steps were highly effective in increasing response rates, and should be repeated in the next collection cycle...</outcomes> "
        ),
        participant = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                abbr = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the participant.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "developmentActivity",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        parTitl = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the translated study title. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                )
            ),
            parents = "titlStmt",
            children = c(),
            title = "Parallel Title",
            description = "Title translated into another language.",
            examples = c(
                "<titl>Politbarometer West [Germany], Partial Accumulation, 1977-1995</titl>",
                "<parTitl>Politbarometer, 1977-1995: Partielle Kumulation</parTitl>"
            )
        ),
        physLoc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Type of file structure: rectangular, hierarchical, two-dimensional, relational.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                recRef = list(
                    type = "xs:IDREF",
                    description = "Link to the appropriate file or recGrp element within a file.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                startPos = list(
                    type = "xs:string",
                    description = "Starting position of variable or data item.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                width = list(
                    type = "xs:string",
                    description = "Number of columns the variable/data item occupies.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                endPos = list(
                    type = "xs:string",
                    description = "End position of variable or data item.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataItem",
            children = c(),
            title = "Physical Location",
            description = c(
                "This is an empty element containing only the attributes listed below. \"RecSegNo\" (the record segment number, deck or card number the variable or data item is located on), and \"fileid\" (an IDREF link to the fileDscr element for the file that includes this physical location).",
                "Remarks: Where the same variable is coded in two different files, e.g., a fixed format file and a relational database file, simply repeat the physLoc element with the alternative location information. Note that if there is no width or ending position, then the starting position should be the ordinal position in the file, and the file would be described as free-format. New attributes will be added as other storage formats are described within the DDI."
            ),
            examples = c(
                "<physLoc type=\"rectangular\" recRef=\"R1\" startPos=\"55\" endPos=\"57\" width=\"3\"/>",
                "<physLoc type=\"hierarchical\" recRef=\"R6\" startPos=\"25\" endPos=\"25\" width=\"1\"/>"
            )
        ),
        point = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "polygon",
            children = c("gringLat", "gringLon"),
            title = "Point",
            description = "0-dimensional geometric primitive, representing a position, but not having extent. In this declaration, point is limited to a longitude/latitude coordinate system.",
            examples = "<point><gridLat>-15.8</gridLat><gridLon>24.0</gridLon></point>"
        ),
        polygon = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "boundPoly",
            children = "point",
            title = "Polygon",
            description = "The minimum polygon that covers a geographical area, and is delimited by at least 4 points (3 sides), in which the last point coincides with the first point.",
            examples = "<polygon><point><gridLat>-15.8</gridLat><gridLon>24.0</gridLon></point><point><gridLat>-41.0</gridLat><gridLon>10.8</gridLon></point><point><gridLat>-15.8</gridLat><gridLon>10.8</gridLon></point><point><gridLat>-15.8</gridLat><gridLon>24.0</gridLon></point></polygon>"
        ),
        postQTxt = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "qstn",
            children = c(),
            title = "PostQuestion Text",
            description = "Text describing what occurs after the literal question has been asked.",
            examples = "<var><qstn><postQTxt>The next set of questions will ask about your financial situation.</postQTxt></qstn></var>"
        ),
        preQTxt = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "qstn",
            children = c(),
            title = "PreQuestion Text",
            description = "Text describing a set of conditions under which a question might be asked.",
            examples = "<var><qstn><preQTxt>For those who did not go away on a holiday of four days or more in 1985...</preQTxt></qstn></var>"
        ),
        ProcStat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "fileTxt",
            children = c(),
            title = "Processing Status",
            description = "Processing status of the file. Some data producers and social science data archives employ data processing strategies that provide for release of data and documentation at various stages of processing.",
            examples = c(
                "<ProcStat>Available from the DDA. Being processed.</ProcStat>",
                "<ProcStat>The principal investigator notes that the data in Public Use Tape 5 are released prior to final cleaning and editing, in order to provide prompt access to the NMES data by the research and policy community.</ProcStat>"
            )
        ),
        prodDate = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "prodStmt",
            children = c(),
            title = "Date of Production",
            description = "Date when the marked-up document/marked-up document source/data collection/other material(s) were produced (not distributed or archived). The ISO standard for dates (YYYY-MM-DD) is recommended for use with the date attribute. Production date for data collection (codeBook/stdyDscr/citation/prodStmt/prodDate) maps to Dublin Core element \"Date\".",
            examples = "<prodDate date=\"1999-01-25\">January 25, 1999</prodDate>"
        ),
        prodPlac = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "prodStmt",
            children = c(),
            title = "Place of Production",
            description = "Address of the archive or organization that produced the work.",
            examples = "<prodPlac>Ann Arbor, MI: Inter-university Consortium for Political and Social Research</prodPlac>"
        ),
        prodStmt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("citation", "docSrc", "fileCitation", "sourceCitation"),
            children = c("language", "producer", "copyright", "license", "prodDate", "prodPlac", "software", "fundAg", "grantNo"),
            title = "Production Statement",
            description = "Part of citation including language, producer, copyright, license, prodDate, prodPlac, software, fundAg, and grantNo. Production statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material for study.",
            examples = c()
        ),
        producer = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation for the producer.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                role = list(
                    type = "xs:string",
                    description = "Distinguishes different stages of involvement in the production process, such as original producer",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the producer.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("prodStmt", "standard"),
            children = c(),
            title = "Producer",
            description = "The producer is the person or organization with the financial or administrative responsibility for the physical processes whereby the document was brought into existence. Producer of data collection (codeBook/stdyDscr/citation/prodStmt/producer) maps to Dublin Core element \"Publisher\". The \"producer\" in the Document Description should be the agency or person that prepared the marked-up document. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = c(
                "<producer abbr=\"ICPSR\" affiliation=\"Institute for Social Research\">Inter-university Consortium for Political and Social Research</producer>",
                "<producer abbr=\"MNPoll\" affiliation=\"Minneapolis Star Tibune Newspaper\" role=\"original producer\">Star Tribune Minnesota Poll</producer>",
                "<producer abbr=\"MRDC\" affiliation=\"University of Minnesota\" role=\"final production\">Machine Readable Data Center</producer>"
            )
        ),
        purpose = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Summary data description references which record the ID values of all elements within the summary data description section of the Study Description which might apply to the nCube. These elements include: time period covered, date of collection, nation or country, geographic coverage, geographic unit, unit of analysis, universe, and kind of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                methrefs = list(
                    type = "xs:IDREFS",
                    description = "Methodology and processing references which record the ID values of all elements within the study methodology and processing section of the Study Description which might apply to the nCube. These elements include information on data collection and data appraisal (e.g., sampling, sources, weighting, data cleaning, response rates, and sampling error estimates).",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                pubrefs = list(
                    type = "xs:IDREFS",
                    description = "Link to publication/citation references and records the ID values of all citations elements in Other Study Description Materials or Other Study-Related Materials that pertain to this nCube.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "nCube",
            children = c(),
            title = "Purpose",
            description = "Explains the purpose for which a particular nCube was created.",
            examples = "<nCube><purpose>Meets reporting requirements for the Federal Reserve Board</purpose></nCube>"
        ),
        qstn = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                ID = list(
                    type = "xs:IDREF",
                    description = "Unique identifier for the question.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                var = list(
                    type = "xs:IDREFS",
                    description = "ID(s) of the variable(s) relating to the question.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                seqNo = list(
                    type = "xs:string",
                    description = "Sequence number of the question.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "References to the elements in the summary data description section of the Study Description which might apply to this question. These elements include: time period covered, date of collection, nation or country, geographic coverage, geographic unit, unit of analysis, universe, and kind of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                responseDomain = list(
                    type = "xs:string",
                    description = "Captures the specific DDI Lifecycle response domain type to facilitate translation between DDI 2 and DDI Lifecycle.",
                    values = c("text", "numeric", "code", "category", "datetime", "geographic", "multiple", "geographicLocationCode", "geographicStructureCode", "scale", "externalCategory", "nominal", "location", "ranking", "distribution", "other"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherResponseDomain = list(
                    type = "xs:NMTOKEN",
                    description = "Other response domain, a term from a controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c("preQTxt", "qstnLit", "postQTxt", "forward", "backward", "ivuInstr"),
            title = "Question",
            description = "The question element may have mixed content. The element itself may contain text for the question, with the subelements being used to provide further information about the question. Alternatively, the question element may be empty and only the subelements used. The element has a unique question ID attribute which can be used to link a variable with other variables where the same question has been asked. This would allow searching for all variables that share the same question ID, perhaps because the questions was asked several times in a panel design.",
            examples = "<var><qstn ID=\"Q125\">When you get together with your friends, would you say you discuss political matters frequently, occasionally, or never?</qstn></var>"
        ),
        qstnLit = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "qstn",
            children = c(),
            title = "Literal Question",
            description = "Text of the actual, literal question asked.",
            examples = "<var><qstn><qstnLit>Why didn't you go away in 1985?</qstnLit></qstn></var>"
        ),
        qualityStatement = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "stdyInfo",
            children = c("standardsCompliance", "otherQualityStatement"),
            title = "Quality Statement",
            description = "This structure consists of two parts, standardsCompliance and otherQualityStatements. In standardsCompliance list all specific standards complied with during the execution of this study. Note the standard name and producer and how the study complied with the standard. Enter any additional quality statements in otherQualityStatements.",
            examples = c()
        ),
        range = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                UNITS = list(
                    type = "xs:string",
                    description = "Integer or real numbers",
                    values = c("INT", "REAL"),
                    default = "INT",
                    optional = TRUE,
                    recommended = FALSE
                ),
                min = list(
                    type = "xs:string",
                    description = "Lowest value that is part of the range",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                minExclusive = list(
                    type = "xs:string",
                    description = "Lowest value immediately outside the range",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                max = list(
                    type = "xs:string",
                    description = "Highest value that is part of the range",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                maxExclusive = list(
                    type = "xs:string",
                    description = "Highest value immediately outside the range",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("cohort", "invalrng", "valrng"),
            children = c(),
            title = "Value Range",
            description = "This is the actual range of values. It is an empty element consisting only of its attributes.",
            examples = c(
                "<range maxExclusive=\"1\"/>",
                "<range min=\"10\" maxExclusive=\"20\"/>"
            )
        ),
        recDimnsn = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                level = list(
                    type = "xs:string",
                    description = "Should be set to \"record\".",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "recGrp",
            children = c("varQnty", "caseQnty", "logRecL"),
            title = "Dimensions (of record)",
            description = "Information about the physical characteristics of the record including the numer of variable (varQnty), number of cases (caseQnty), and record length (logRecL).",
            examples = c()
        ),
        recGrp = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                recGrp = list(
                    type = "xs:IDREFS",
                    description = "IDs of the subsidiary record groups which nest underneath; this allows for the encoding of a hierarchical structure of record groups.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                rectype = list(
                    type = "xs:string",
                    description = "Type of record, e.g., \"A records\" or \"Household records.\"",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                keyvar = list(
                    type = "xs:IDREFS",
                    description = "Link to other record types",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                rtypeloc = list(
                    type = "xs:string",
                    description = "Starting column location of the record type indicator variable on each record of the data file.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                rtypewidth = list(
                    type = "xs:string : 1",
                    description = "Width, for files with many different record types.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                rtypevtype = list(
                    type = "xs:string",
                    description = "Type of the indicator variable.",
                    values = c("numeric", "character"),
                    default = "numeric",
                    optional = TRUE,
                    recommended = FALSE
                ),
                recidvar = list(
                    type = "xs:string",
                    description = "Variable that identifies the record group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "fileStrc",
            children = c("labl", "recDimnsn"),
            title = "Record or Record Group",
            description = "Used to describe record groupings if the file is hierarchical or relational. In a hierarchical study consisting of individual and household records, the \"keyvar\" on the person record will indicate the household to which it belongs.",
            examples = "<fileStrc type=\"hierarchical\"><recGrp rectype=\"Person\" keyvar=\"HHDID\"><labl>CPS 1999 Person-Level Record</labl><recDimnsn><varQnty>133</varQnty><caseQnty>1500</caseQnty><logRecL>852</logRecL></recDimnsn></recGrp></fileStrc>"
        ),
        recNumTot = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dimensns",
            children = c(),
            title = "Overall Number of Records",
            description = "Overall record count in file. Particularly helpful in instances such as files with multiple cards/decks or records per case.",
            examples = "<dimensns><recNumTot>2400</recNumTot></dimensns>"
        ),
        recPrCas = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dimensns",
            children = c(),
            title = "Records per Case",
            description = "Records per case in the file. This element should be used for card-image data or other files in which there are multiple records per case.",
            examples = c(
                "<dimensns><recPrCas>5</recPrCas></dimensns>",
                "<dimensns><recPrCas>5</recPrCas></dimensns>"
            )
        ),
        referencePeriod = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                event = list(
                    type = "xs:string",
                    description = "Type of event, one of \"start\", \"end\" or \"single\".",
                    values = c("start", "end", "single"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sampleFrame",
            children = c(),
            title = "Reference Period",
            description = "Indicates the period of time in which the sampling frame was actually used for the study in question. Use ISO 8601 date/time formats to enter the relevant date(s).",
            examples = "<referencePeriod event=\"single\">2009-06-01</referencePeriod> "
        ),
        relMat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                callno = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                label = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                media = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                type = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "othrStdyMat",
            children = c(),
            title = "Related Materials",
            description = "Describes materials related to the study description, such as appendices, additional information on sampling found in other documents, etc. Can take the form of bibliographic citations. This element can contain either PCDATA or a citation or both, and there can be multiple occurrences of both the citation and PCDATA within a single element. May consist of a single URI or a series of URIs comprising a series of citations/references to external materials which can be objects as a whole (journal articles) or parts of objects (chapters or appendices in articles or documents).",
            examples = c(
                "<relMat> Full details on the research design and procedures, sampling methodology, content areas, and questionnaire design, as well as percentage distributions by respondent's sex, race, region, college plans, and drug use, appear in the annual ISR volumes MONITORING THE FUTURE: QUESTIONNAIRE RESPONSES FROM THE NATION'S HIGH SCHOOL SENIORS.</relMat>",
                "<relMat>Current Population Survey, March 1999: Technical Documentation includes an abstract, pertinent information about the file, a glossary, code lists, and a data dictionary. One copy accompanies each file order. When ordered separately, it is available from Marketing Services Office, Customer Service Center, Bureau of the Census, Washington, D.C. 20233. </relMat>",
                "<relMat>A more precise explanation regarding the CPS sample design is provided in Technical Paper 40, The Current Population Survey: Design and Methodology. Chapter 5 of this paper provides documentation on the weighting procedures for the CPS both with and without supplement questions.</relMat>"
            )
        ),
        relPubl = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "othrStdyMat",
            children = c(),
            title = "Related Publications",
            description = "Bibliographic and access information aboutvarticles and reports based on the data in this collection. Can take the form of bibliographic citations. ",
            examples = c(
                "<relPubl>Economic Behavior Program Staff. SURVEYS OF CONSUMER FINANCES. Annual volumes 1960 through 1970. Ann Arbor, MI: Institute for Social Research.</relPubl>",
                "<relPubl>Data from the March Current Population Survey are published most frequently in the Current Population Reports P- 20 and P- 60 series. These reports are available from the Superintendent of Documents, U. S. Government Printing Office, Washington, DC 20402. They also are available on the INTERNET at http://www. census. gov. Forthcoming reports will be cited in Census and You, the Monthly Product Announcement (MPA), and the Bureau of the Census Catalog and Guide. </relPubl>"
            )
        ),
        relStdy = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "othrStdyMat",
            children = c(),
            title = "Related Studies",
            description = "Information on the relationship of the current data collection to others (e.g., predecessors, successors, other waves or rounds) or to other editions of the same file. This would include the names of additional data collections generated from the same data collection vehicle plus other collections directed at the same general topic. Can take the form of bibliographic citations. ",
            examples = "<relStdy>ICPSR distributes a companion study to this collection titled FEMALE LABOR FORCE PARTICIPATION AND MARITAL INSTABILITY, 1980: [UNITED STATES] (ICPSR 9199).</relStdy>"
        ),
        resInstru = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Type of Research Instrument",
            description = "The type of data collection instrument used. \"Structured\" indicates an instrument in which all respondents are asked the same questions/tests, possibly with precoded answers. If a small portion of such a questionnaire includes open-ended questions, provide appropriate comments. \"Semi-structured\" indicates that the research instrument contains mainly open-ended questions. \"Unstructured\" indicates that in-depth interviews were conducted. The use of the attribute \"type\" as a means of specifiying a controlled vocabulary concept is DEPRECATED. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevent concept.",
            examples = "<resInstru><concept vocab=\"TypeOfInstrument\" vocabURI=\"http://www.ddialiance.org/Specification/DDI-CV/TypeOfInstruent_1.1.html\" vocabInstanceURI\"http://www.ddialiance.org/Specification/DDI-CV/TypeOfInstruent_1.1.html#Questionnaire.Structured\">Questionnaire.Structured</concept>structured</resInstru>"
        ),
        resource = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "developmentActivity",
            children = c("typeOfDataSrc", "dataSrc", "srcOrig", "srcChar", "srcDocu"),
            title = c(),
            description = c(),
            examples = c()
        ),
        respRate = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "anlyInfo",
            children = c(),
            title = "Response Rate",
            description = "The percentage of sample members who provided information. This may include a broader description of stratified response rates, information affecting resonse rates etc.",
            examples = c(
                "<respRate>For 1993, the estimated inclusion rate for TEDS-eligible providers was 91 percent, with the inclusion rate for all treatment providers estimated at 76 percent (including privately and publicly funded providers).</respRate>",
                "<respRate>The overall response rate was 82%, although retail firms with an annual sales volume of more than $5,000,000 were somewhat less likely to respond.</respRate>"
            )
        ),
        respUnit = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("nCube", "var"),
            children = c(),
            title = "Response Unit",
            description = "Provides information regarding who provided the information contained within the variable/nCube, e.g., respondent, proxy, interviewer. This element may be repeated only to support multiple language expressions of the content.",
            examples = c(
                "<var><respUnit>Head of household</respUnit></var>",
                "<nCube><respUnit>Head of household</respUnit></nCube>"
            )
        ),
        restrctn = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the Access statement. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                )
            ),
            parents = "useStmt",
            children = c(),
            title = "Restrictions",
            description = "Any restrictions on access to or use of the collection such as privacy certification or distribution restrictions should be indicated here. These can be restrictions applied by the author, producer, or disseminator of the data collection. If the data are restricted to only a certain class of user, specify which type.",
            examples = c(
                "<restrctn> In preparing the data file(s) for this collection, the National Center for Health Statistics (NCHS) has removed direct identifiers and characteristics that might lead to identification of data subjects. As an additional precaution NCHS requires, under Section 308(d) of the Public Health Service Act (42 U.S.C. 242m), that data collected by NCHS not be used for any purpose other than statistical analysis and reporting. NCHS further requires that analysts not use the data to learn the identity of any persons or establishments and that the director of NCHS be notified if any identities are inadvertently discovered. ICPSR member institutions and other users ordering data from ICPSR are expected to adhere to these restrictions.</restrctn>",
                "<restrctn> ICPSR obtained these data from the World Bank under the terms of a contract which states that the data are for the sole use of ICPSR and may not be sold or provided to third parties outside of ICPSR membership. Individuals at institutions that are not members of the ICPSR may obtain these data directly from the World Bank.</restrctn>"
            )
        ),
        rspStmt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("citation", "docSrc", "fileCitation", "sourceCitation"),
            children = c("AuthEnty", "othId"),
            title = "Responsibility Statement",
            description = "Part of citation covering author (AuthEnty) and collaborators (othID). Responsibility for the creation of the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material for study.",
            examples = c()
        ),
        sampleFrame = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c("sampleFrameName", "labl", "txt", "validPeriod", "custodian", "useStmt", "universe", "frameUnit", "referencePeriod", "updateProcedure"),
            title = "Sample Frame",
            description = "Sample frame describes the sampling frame used for identifying the population from which the sample was taken. For example, a telephone book may be a sample frame for a phone survey. In addition to the name, label and text describing the sample frame, this structure lists who maintains the sample frame, the period for which it is valid, a use statement, the universe covered, the type of unit contained in the frame as well as the number of units available, the reference period of the frame and procedures used to update the frame. Use multiple use statements to provide different uses under different conditions. Repeat elements within the use statement to support multiple languages.",
            examples = c()
        ),
        sampleFrameName = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "sampleFrame",
            children = c(),
            title = "Sample Frame Name",
            description = "Name of the sample frame.",
            examples = "<sampleFrameName>City of St. Paul Directory</sampleFrameName> "
        ),
        sampleSize = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "targetSampleSize",
            children = c(),
            title = "Sample Size",
            description = "This element provides the targeted sample size in integer format.",
            examples = "<sampleSize>385</sampleSize>"
        ),
        sampleSizeFormula = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "targetSampleSize",
            children = c(),
            title = "Sample Size Formula",
            description = "This element includes the formula that was used to determine the sample size.",
            examples = "<sampleSizeFormula>n0=Z2pq/e2=(1.96)2(.5)(.5)/(.05)2=385 individuals</sampleSizeFormula>"
        ),
        sampProc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c(),
            title = "Sampling Procedure",
            description = "The type of sample and sample design used to select the survey respondents to represent the population. May include reference to the target sample size and the sampling fraction.",
            examples = c(
                "<sampProc>National multistage area probability sample</sampProc>",
                "<sampProc>Simple random sample</sampProc>",
                "<sampProc>Stratified random sample</sampProc>",
                "<sampProc>Quota sample</sampProc>",
                "<sampProc>The 8,450 women interviewed for the NSFG, Cycle IV, were drawn from households in which someone had been interviewed for the National Health Interview Survey (NHIS), between October 1985 and March 1987.</sampProc>",
                "<sampProc>Samples sufficient to produce approximately 2,000 families with completed interviews were drawn in each state. Families containing one or more Medicaid or uninsured persons were oversampled. XHTML content may be used for formatting.</sampProc>"
            )
        ),
        security = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "ISO standard for dates (YYYY-MM-DD) is recommended.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("nCube", "var"),
            children = c(),
            title = "Security",
            description = "Provides information regarding levels of access, e.g., public, subscriber, need to know.",
            examples = c(
                "<var><security date=\"1998-05-10\"> This variable has been recoded for reasons of confidentiality. Users should contact the archive for information on obtaining access.</security></var>",
                "<var><security date=\"1998-05-10\">Variable(s) within this nCube have been recoded for reasons of confidentiality. Users should contact the archive for information on obtaining access.</security></var>"
            )
        ),
        selector = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "usage",
            children = c(),
            title = "Selector",
            description = "Identifies a collection of elements in which a controlled vocabulary is used. This is a simplified XPath which must correspond to the actual instance in which it occurs, which is to say that the fully qualified element names here must correspond to those in the instance. This XPath can only identify elements and does not allow for any predicates. The XPath must either be rooted or deep.",
            examples = " <selector>/codeBook/stdyDscr/method/dataColl/timeMeth</selector> "
        ),
        serInfo = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "serStmt",
            children = c(),
            title = "Series Information",
            description = "Contains a history of the series and a summary of those features that apply to the series as a whole.",
            examples = "<serInfo>The Current Population Survey (CPS) is a household sample survey conducted monthly by the Census Bureau to provide estimates of employment, unemployment, and other characteristics of the general labor force, estimates of the population as a whole, and estimates of various subgroups in the population. The entire non-institutionalized population of the United States is sampled to obtain the respondents for this survey series.</serInfo>"
        ),
        serName = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                abbr = list(
                    type = "xs:string",
                    description = "Abbreviation for the series name.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "serStmt",
            children = c(),
            title = "Series Name",
            description = "The name of the series to which the work belongs.",
            examples = "<serName abbr=\"CPS\">Current Population Survey Series</serName>"
        ),
        serStmt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                URI = list(
                    type = "xs:string",
                    description = "A central Internet repository for information on the series.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("citation", "docSrc", "fileCitation", "sourceCitation"),
            children = c("serName", "serInfo"),
            title = "Series Statement",
            description = "Series statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material for study. Repeat this field if the study is part of more than one series. Repetition of the internal content should be used to support multiple languages only.",
            examples = c()
        ),
        setAvail = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                media = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                callno = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                label = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataAccs",
            children = c("typeOfSetAvailability", "accsPlac", "origArch", "avlStatus", "collSize", "complete", "fileQnty", "notes"),
            title = "Data Set Availability",
            description = "Information on availability and storage of the collection. The \"media\" attribute may be used in combination with any of the subelements. See Location of Data Collection. Use of the \"type\" attribute has been DEPRECATED. Use the element typeOfSetAvailability.",
            examples = c()
        ),
        software = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "Date (if any) for the software release. ISO standard for dates (YYYY-MM-DD) is recommended",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                version = list(
                    type = "xs:string",
                    description = "Software version number.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("fileTxt", "prodStmt"),
            children = c(),
            title = "Software used in Production",
            description = "Software used to produce the work.",
            examples = c(
                "<docDscr><citation><prodStmt><software version=\"1.0\">MRDC Codebook Authoring Tool</software></prodStmt></citation></docDscr>",
                "<docDscr><citation><prodStmt><software version=\"8.0\">Arbortext Adept Editor</software></prodStmt></citation></docDscr>",
                "<docDscr><docSrc><prodStmt><software version=\"4.0\">PageMaker</software></prodStmt></docSrc></docDscr>",
                "<stdyDscr><citation><prodStmt><software version=\"6.12\">SAS</software></prodStmt></citation></stdyDscr>",
                "<fileTxt><software version=\"6.12\">The SAS transport file was generated by the SAS CPORT procedure.</software></fileTxt>"
            )
        ),
        sourceCitation = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                MARCURI = list(
                    type = "xs:string",
                    description = "MAchine Readable Citation URI, link to the MARC record for the citation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sources",
            children = c(),
            title = "Source Citation",
            description = "This complex element allows the inclusion of a standard citation for the sources used in collecting and creating the dataset.",
            examples = "<sourceCitation><titlStmt><titl>Tenth Decennial Census of the United States, 1880. Volume I. Statistics of the Population of the United States at the Tenth Census.</titl></titlStmt><rspStmt><AuthEnty affiliation=\"U.S. Department of Commerce\">United States Census Bureau</AuthEnty></rspStmt><prodStmt><producer>Government Printing Office</producer><prodDate>1883</prodDate></prodStmt></sourceCitation> "
        ),
        sources = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("dataColl", "sources"),
            children = c("typeOfDataSrc", "dataSrc", "sourceCitation", "srcOrig", "srcChar", "srcDocu", "sources"),
            title = "Sources Statement",
            description = "Description of sources used for the data collection. The element is nestable so that the sources statement might encompass a series of discrete source statements, each of which could contain the facts about an individual source. Provides the type of data source used such as Register Records Accounts, Research Data, Biological Samples, etc. Use of an external controlled vocabulary is recommended. DDI provides Data Source Type. This element maps to Dublin Core element \"Source\".",
            examples = c()
        ),
        southBL = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "geoBndBox",
            children = c(),
            title = "South Bounding Latitude",
            description = "The southernmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive east and positive north), is: -90,0 <= South Bounding Latitude Value <= 90,0 ; South Bounding Latitude Value <= North Bounding Latitude Value",
            examples = "<southBL>57.987915</southBL>"
        ),
        specificElements = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                refs = list(
                    type = "xs:IDREFS",
                    description = "IDs of the specific elements.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                authorizedCodeValue = list(
                    type = "xs:NMTOKEN",
                    description = "A valid code value corresponding to the meaning of the content in the element or attribute when the identified element or attribute does not use an actual valid value from the controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "usage",
            children = c(),
            title = "Specific Elements",
            description = "Identifies a collection of specific elements via their identifiers in the refs attribute, which allows for a tokenized list of identifier values which must correspond to identifiers which exist in the instance.",
            examples = " <specificElements refs=\"ICPSR4328timeMeth\" authorizedCodeValue=\"CrossSection\"/> "
        ),
        specPerm = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                required = list(
                    type = "xs:string",
                    description = "Facilitates machine processing of this element",
                    values = c("yes", "no"),
                    default = "yes",
                    optional = TRUE,
                    recommended = FALSE
                ),
                formNo = list(
                    type = "xs:string",
                    description = "Number or ID of the form that the user must fill out.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "URN or URL for online access to a special permissions form.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "useStmt",
            children = c(),
            title = "Special Permissions",
            description = "This element is used to determine if any special permissions are required to access a resource.",
            examples = "<specPerm formNo=\"4\">The user must apply for special permission to use this dataset locally and must complete a confidentiality form.</specPerm>"
        ),
        srcChar = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("resource", "sources"),
            children = c(),
            title = "Characteristics of Source Noted",
            description = "Assessment of characteristics and quality of source material. May not be relevant to survey data. This element may be repeated to support multiple language expressions of the content.",
            examples = "<srcChar>Print source document. Page 26 torn causing loss of some definitional material. Related sources were used to make up for this loss.</srcChar>"
        ),
        srcDocu = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("resource", "sources"),
            children = c(),
            title = "Documentation and Access to Sources",
            description = "Level of documentation of the original sources. May not be relevant to survey data. This element may be repeated to support multiple language expressions of the content.",
            examples = c()
        ),
        srcOrig = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("resource", "sources"),
            children = c(),
            title = "Origins of Sources",
            description = "For historical materials, information about the origin(s) of the sources and the rules followed in establishing the sources should be specified. May not be relevant to survey data. This element may be repeated to support multiple language expressions of the content.",
            examples = c()
        ),
        standard = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "standardsCompliance",
            children = c("producer", "standardName"),
            title = "Standard",
            description = "Describes a standard with which the study complies.",
            examples = c()
        ),
        standardName = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "(xs:dateTime | xs:date | xs:gYearMonth | xs:gYear)",
                    description = "Date when the standard was published.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                version = list(
                    type = "xs:string",
                    description = "Specific version of the standard with which the study is compliant.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:anyURI",
                    description = "URI for the actual standard.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "standard",
            children = c(),
            title = "Standard Name",
            description = "Contains the name of the standard with which the study complies.",
            examples = "<standardName date=\"2009-10-18\" version=\"3.1\" URI=\"http://www.ddialliance.org/Specification/DDI-Lifecycle/3.1/\">Data Documentation Initiative</standardName> "
        ),
        standardsCompliance = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "qualityStatement",
            children = c("standard", "complianceDescription"),
            title = "Standards Compliance",
            description = "This section lists all specific standards complied with during the execution of this study. Specify the standard(s)' name(s) and producer(s) and describe how the study complied with each standard in complianceDescription. Enter any additional quality statements in otherQualityStatement.",
            examples = "<standardsCompliance><standard><standardName>Data Documentation Initiative</standardName><producer>DDI Alliance</producer></standard><complianceDescription>Study metadata was created in compliance with the Data Documentation Initiative (DDI) standard</complianceDescription></standardsCompliance> "
        ),
        stdCatgry = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "Indicates the version of the code in place at the time of the study.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "URN or URL that can be used to obtain an electronic list of the category codes.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access and Metadata Access section that describe access conditions.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c(),
            title = "Standard Categories",
            description = "Standard category codes used in the variable, like industry codes, employment codes, or social class codes.",
            examples = "<var><stdCatgry date=\"1981\" source=\"producer\">U. S. Census of Population and Housing, Classified Index of Industries and Occupations </stdCatgry></var>"
        ),
        stdyClas = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "method",
            children = c(),
            title = "Class of the Study",
            description = "Generally used to give the data archive's class or study status number, which indicates the processing status of the study. May also be used as a text field to describe processing status. This element may be repeated to support multiple language expressions of the content. The use of the attribute \"type\" as a means of specifiying a controlled vocabulary concept is DEPRECATED. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevent concept.",
            examples = c(
                "<stdyClas><concept vocab=\"ICPSR\" vocabURI=\"http://icpar.umich.edu/ICPSRvocabularies/StudyClass\" vocabInstanceURI=\"http://icpar.umich.edu/ICPSRvocabularies/StudyClass#Class_II\">Class_II</concept>ICPSR Class II</stdyClas>",
                "<stdyClas><concept vocab=\"DDA-StudyClass\" vocabURI=\"http://dda.dk/DDAvocabularies/StudyClass\" vocabInstanceURI=\"http://dda.dk/DDAvocabularies/StudyClass#Class_C\"</concept>DDA Class C</stdyClas>",
                "<stdyClas><concept vocab=\"DDA-Processing\" vocabURI=\"http://dda.dk/DDAvocabularies/Processing\" vocabInstanceURI=\"http://dda.dk/DDAvocabularies/Processing#inProcess\">inProcess</concept>Available from the DDA. Being processed.</stdyClas>"
            )
        ),
        stdyDscr = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                access = list(
                    type = "xs:IDREFS",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "codeBook",
            children = c("citation", "studyAuthorization", "stdyInfo", "studyDevelopment", "method", "dataAccs", "metadataAccs", "othrStdyMat", "notes"),
            title = "Study Description",
            description = "The Study Description consists of information about the data collection, study, or compilation that the DDI-compliant documentation file describes. This section includes information about how the study should be cited, who collected or compiled the data, who distributes the data, keywords about the content of the data, summary (abstract) of the content of the data, data collection methods and processing, etc. Note that some content of the Study Description's Citation -- e.g., Responsibility Statement -- may be identical to that of the Documentation Citation. This is usually the case when the producer of a data collection also produced the print or electronic codebook for that data collection.",
            examples = c()
        ),
        stdyInfo = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyDscr",
            children = c("studyBudget", "subject", "abstract", "sumDscr", "qualityStatement", "notes", "exPostEvaluation"),
            title = "Study Scope",
            description = "This section contains information about the data collection's scope across several dimensions, including substantive content, geography, and time.",
            examples = c()
        ),
        studyAuthorization = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "(xs:dateTime | xs:date | xs:gYearMonth | xs:gYear)",
                    description = "Date of authorization.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "stdyDscr",
            children = c("authorizingAgency", "authorizationStatement"),
            title = "Study Authorization",
            description = "Provides structured information on the agency that authorized the study, and an authorization statement.",
            examples = "<studyAuthorization date=\"2010-11-04\"><authorizingAgency affiliation=\"University of Georgia\" abbr=\"HSO\">Human Subjects Office</authorizingAgency><authorizationStatement>Statement of authorization issued bu OUHS on 2010-11-04</authorizationStatement></studyAuthorization> "
        ),
        studyBudget = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyInfo",
            children = c(),
            title = "Study Budget",
            description = "Describe the budget of the project in as much detail as needed. Use XHTML structure elements to identify discrete pieces of information in a way that facilitates direct transfer of information on the study budget between DDI 2 and DDI Lifecycle structures.",
            examples = "<studyBudget>The budget for the study covers a 5 year award period distributed between direct and indirect costs including: Staff, ...</studyBudget> "
        ),
        studyDevelopment = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyDscr",
            children = "developmentActivity",
            title = "Study Development",
            description = "Describe the process of study development as a series of development activities. These activities can be typed using a controlled vocabulary. Describe the activity, listing participants with their role and affiliation, resources used (sources of information), and the outcome of the development activity.",
            examples = "<developmentActivity type=\"checkDataAvailability\"><description>A number of potential sources were evaluated for content, consistency and quality</description><participant affiliation=\"NSO\" role=\"statistician\">John Doe</participant></resource><dataSrc>Study S</dataSrc><srcOrig>Collected in 1970 using unknown sampling method</srcOrig><srcChar>Information incomplete missing X province</srcChar></resource><outcome>Due to quality issues this was determined not to be a viable source of data for the study</outcome></developmentActivity>"
        ),
        subject = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyInfo",
            children = c("keyword", "topcClas"),
            title = "Subject Information",
            description = "Subject information describing the data collection's intellectual content. Supports the use of a list of keyword and a list of topic classifications.",
            examples = c()
        ),
        subTitl = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "titlStmt",
            children = c(),
            title = "Subtitle",
            description = "A secondary title used to amplify or state certain limitations on the main title. It may repeat information already in the main title. ",
            examples = c(
                "<titl>Monitoring the Future: A Continuing Study of American Youth, 1995</titl>",
                "<subTitl>A Continuing Study of American Youth, 1995</subTitl>",
                "<titl>Census of Population, 1950 [United States]: Public Use Microdata Sample</titl>",
                "<subTitl>Public Use Microdata Sample</subTitl>"
            )
        ),
        sumDscr = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "stdyInfo",
            children = c("timePrd", "collDate", "nation", "geogCover", "geogUnit", "geoBndBox", "boundPoly", "anlyUnit", "universe", "dataKind", "generalDataFormat"),
            title = "Summary Data Description",
            description = c(
                "Information about the and geographic coverage of the study and unit of analysis.",
                "Replication of the element geoBndBox is NOT recommended. The purpose of a bounding box is to support high level geographic point search systems. Most search systems of this type do not handle multiple instances of a bounding box. The bounding box should represent the full geographic coverage extent of the of the overall datasets being described by the Codebook instance. If there is a desire to provide the equivilent of a bounding box for each of multiple summary descriptions, use of the boundPoly is recommended. First provide a geoBndBox for the full area covered by the study. Then provide a boundPoly for each geographic area defined with a separate sumDscr. Note that when describing a bounding box using a boundPoly description the four corner points are described. The starting point and end point should match (closing the polygon).",
                "Replication of sumDscr is useful when bundling specifics like timePrd, nation, and universe for specific samples within a larger project. A clear example of this is description for the various IPUMS project that harmonize multiple samples of census, health, and related data. IPUMS identified the coverage of individual samples within a project using a combination of these three elements to be able to differentiate between samples."
            ),
            examples = "<sumDscr><timeProd date=\"2014\">2014</timeProd><nation>Burkino Faso</nation><universe>Women</universe></sumDscr><sumDscr><timeProd date=\"2014\">2014</timeProd><nation>Burkino Faso</nation><universe>Children</universe></sumDscr><sumDscr><timeProd date=\"2018\">2018</timeProd><nation>Burkino Faso</nation><universe>Women</universe></sumDscr><sumDscr><timeProd date=\"2018\">2018</timeProd><nation>India</nation><universe>Women</universe></sumDscr>"
        ),
        sumStat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                wgtd = list(
                    type = "xs:string",
                    description = "Statistics are weighted or not.",
                    values = c("wgtd", "not-wgtd"),
                    default = "not-wgtd",
                    optional = TRUE,
                    recommended = FALSE
                ),
                wgt_var = list(
                    type = "xs:IDREFS",
                    description = c("A reference to the variable containing the weight used.", "(the actual attribute is named \"wgt-var\" in the final XML file.)"),
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                weight = list(
                    type = "xs:IDREFS",
                    description = "IDs of the weight element(s) in the study description.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                type = list(
                    type = "xs:string",
                    description = "Type of statistics being shown: mean, median, mode, valid cases, invalid cases, minimum, maximum, or standard deviation.",
                    values = c("mean", "medn", "mode", "vald", "invd", "min", "max", "stdev", "other"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access and Metadata Access section that describe access conditions for this statistic.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherType = list(
                    type = "xs:NMTOKEN",
                    description = "A value taken from a controlled vocabulary, if option for \"type\" is other. This option should only be used when applying a controlled vocabulary to this attribute. Use the complex element controlledVocabUsed to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c(),
            title = "Summary Statistics",
            description = "One or more statistical measures that describe the responses to a particular variable and may include one or more standard summaries, e.g., minimum and maximum values, median, mode, etc.",
            examples = c(
                "<var><sumStat type=\"min\">0</sumStat></var>",
                "<var><sumStat type=\"max\">9</sumStat></var>",
                "<var><sumStat type=\"median\">4</sumStat></var>"
            )
        ),
        table = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                frame = list(
                    type = "xs:string",
                    description = "",
                    values = c("top", "bottom", "topbot", "all", "sides", "none"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                colsep = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                rowsep = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                pgwide = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("key", "notes", "otherMat", "txt"),
            children = c(),
            title = "Table",
            description = c(),
            examples = c()
        ),
        targetSampleSize = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c("sampleSize", "sampleSizeFormula"),
            title = "Target Sample Size",
            description = "Provides both the target size of the sample (this is the number in the original sample, not the number of respondents) as well as the formula used for determining the sample size.",
            examples = c()
        ),
        timeMeth = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                method = list(
                    type = "xs:string",
                    description = "DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataColl",
            children = c(),
            title = "Time Method",
            description = "The time method or time dimension of the data collection. The use of the attribute \"method\" as a means of specifiying a controlled vocabulary concept is DEPRECATED. To specify the use of a Controlled Vocabulary or standard concept use the internal element \"concept\". If multiple concepts are needed the parent element should be replicated. Internal text related to each concept should be allocated to accompany the relevent concept. For forward-compatibility, DDI Lifecycle XHTML tags may be used in this element.",
            examples = c(
                "<timeMeth><concept vocab=\"TimeMethod\" vocabURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0.xml\" vocabInstanceURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0#Longitudinal.Panel\">Longitudinal.Panel</concept>panel survey</timeMeth>",
                "<timeMeth><concept vocab=\"TimeMethod\" vocabURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0.xml\" vocabInstanceURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0#CrossSection\">CrossSection</concept>cross-section</timeMeth>",
                "<timeMeth><concept vocab=\"TimeMethod\" vocabURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0.xml\" vocabInstanceURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0#Longitudinal.TrendRepeatedCrossSection\">Longitudinal.TrendRepeatedCrossSection</concept>trend study</timeMeth>",
                "<timeMeth><concept vocab=\"TimeMethod\" vocabURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0.xml\" vocabInstanceURI=\"http://www.ddialliance.org/Specification/DDI-CV/TimeMethod_1.2_Genericode1.0_DDI-CVProfile1.0#TimeSeries\">TimeSeries</concept>time-series</timeMeth>"
            )
        ),
        timePrd = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "ISO standard for dates (YYYY-MM-DD) is recommended",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                event = list(
                    type = "xs:string",
                    description = "Type of event.",
                    values = c("start", "end", "single"),
                    default = "single",
                    optional = TRUE,
                    recommended = FALSE
                ),
                cycle = list(
                    type = "xs:string",
                    description = "Relevant cycle, wave, or round of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sumDscr",
            children = c(),
            title = "Time Period Covered",
            description = "The time period to which the data refer. This item reflects the time period covered by the data, not the dates of coding or making documents machine-readable or the dates the data were collected. Also known as span. Maps to Dublin Core element \"Coverage\". Inclusion of this element is recommended. ",
            examples = c(
                "<timePrd event=\"start\" date=\"1998-05-01\">May 1, 1998</timePrd>",
                "<timePrd event=\"end\" date=\"1998-05-31\">May 31, 1998</timePrd>"
            )
        ),
        titl = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language used in the title. Use of ISO (www.iso.org) language codes is recommended.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                )
            ),
            parents = c("table", "titlStmt"),
            children = c(),
            title = "Title",
            description = "Full authoritative title for the work at the appropriate level: marked-up document; marked-up document source; study; other material(s) related to study description; other material(s) related to study. The study title will in most cases be identical to the title for the marked-up document. A full title should indicate the geographic scope of the data collection as well as the time period covered. Title of data collection (codeBook/stdyDscr/citation/titlStmt/titl) maps to Dublin Core element \"Title\". This element is required in the Study Description citation. ",
            examples = c(
                "<titl>Domestic Violence Experience in Omaha, Nebraska, 1986-1987</titl>",
                "<titl>Census of Population, 1950 [United States]: Public Use Microdata Sample</titl>",
                "<titl>Monitoring the Future: A Continuing Study of American Youth, 1995</titl>"
            )
        ),
        titlStmt = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("citation", "docSrc", "fileCitation", "sourceCitation"),
            children = c("titl", "subTitl", "altTitl", "parTitl", "IDNo"),
            title = "Title Statement",
            description = "Title statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other materials; other materials for study.",
            examples = c()
        ),
        topcClas = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the keyword. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = TRUE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "subject",
            children = c(),
            title = "Topic Classification",
            description = "The classification field indicates the broad substantive topic(s) that the data cover. Library of Congress subject terms may be used here. Maps to Dublin Core element \"Subject\". Inclusion of this element in the codebook is recommended.",
            examples = c(
                "<topcClas vocab=\"LOC Subject Headings\" vocabURI=\"http://www.loc.gov/catdir/cpso/lcco/lcco.html\" vocabInstanceURI=\"http://www.loc.gov/catdir/cpso/lcco/lcco#PublicOpinion--California--Statistics\">Public opinion -- California -- Statistics</topcClas>",
                "<topcClas vocab=\"LOC Subject Headings\" vocabURI=\"http://www.loc.gov/catdir/cpso/lcco/lcco.html\" vocabInstanceURI=\"http://www.loc.gov/catdir/cpso/lcco/lcco#Elections--California\">Elections -- California</topcClas>"
            )
        ),
        TotlResp = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "var",
            children = c(),
            title = "Total Responses",
            description = "The number of responses to this variable. This element might be used if the number of responses does not match added case counts. It may also be used to sum the frequencies for variable categories.",
            examples = c(
                "<var><TotlResp>1,056</TotlResp></var>",
                "<var><TotlResp>There are only 725 responses to this question since it was not asked in Tanzania.</TotlResp></var>"
            )
        ),
        txt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                level = list(
                    type = "xs:string",
                    description = "Level to which the element applies.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Specific dates, universes, or other information encoded in the study description.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("anlyUnit", "anlysUnit", "catgry", "catgryGrp", "codingInstructions", "collMode", "collectorTraining", "dataAppr", "dataChck", "dataKind", "dataProcessing", "evaluationProcess", "frameUnit", "frequenc", "geogCover", "geogUnit", "instrumentDevelopment", "nCube", "nCubeGrp", "nation", "otherMat", "resInstru", "sampProc", "sampleFrame", "srcOrig", "stdyClas", "timeMeth", "unitType", "universe", "var", "varGrp", "weight"),
            children = c(),
            title = "Descriptive Text",
            description = "Lengthier description of the parent element.",
            examples = c(
                "<varGrp type=\"subject\"><txt>The following five variables refer to respondent attitudes toward national environmental policies: air pollution, urban sprawl, noise abatement, carbon dioxide emissions, and nuclear waste.</txt></varGrp>",
                "<nCubeGrp type=\"subject\"><txt>The following four nCubes are grouped to present a cross tabulation of the variables Sex, Work experience in 1999, and Income in 1999.</txt></nCubeGrp>",
                "<var><txt>Total population for the agency for the year reported.</txt></var>",
                "<catgryGrp><txt>When the respondent indicated his political party reference, his response was coded on a scale of 1-99 with parties with a left-wing orientation coded on the low end of the scale and parties with a right-wing orientation coded on the high end of the scale. Categories 90-99 were reserved miscellaneous responses.</txt></catgryGrp>",
                "<catgry><txt>Inap., question not asked in Ireland, Northern Ireland, and Luxembourg.</txt></catgry>",
                "<nCube><txt>Detailed poverty status for age cohorts over a period of five years, to be used in determining program eligibility</txt></nCube>",
                "<otherMat URI=\"http://www.icpsr.umich.edu/..\"><txt>This is a PDF version of the original questionnaire provided by the principal investigator.</txt></otherMat>",
                "<otherMat><txt>Glossary of Terms. Below are terms that may prove useful in working with the technical documentation for this study.. </txt></otherMat>",
                "<otherMat><txt>This is a PDF version of the original questionnaire provided by the principal investigator.</txt></otherMat>"
            )
        ),
        typeOfAccess = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("dataAccs", "metadataAccs"),
            children = c(),
            title = "Type of access.",
            description = "Provide a specific type of access to the study. Use of a external controlled vocabulary is strongly recommended.",
            examples = c()
        ),
        typeOfCodingInstruction = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "codingInstructions",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        typeOfDataSrc = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("resource", "sources"),
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        typeOfDevelopmentActivity = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "developmentActivity",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        typeOfExPostEvaluation = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "exPostEvaluation",
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        typeOfSetAvailability = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                vocab = list(
                    type = "xs:string",
                    description = "Indicates the controlled vocabulary, if any, used in the element, e.g., LCSH (Library of Congress Subject Headings), MeSH (Medical Subject Headings), etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabURI = list(
                    type = "xs:string",
                    description = "Specifies the location for the full controlled vocabulary.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vocabInstanceURI = list(
                    type = "xs:string",
                    description = "Identification URI of the term/code within the controlled vocabulary, if available.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("otherMat", "setAvail"),
            children = c(),
            title = c(),
            description = c(),
            examples = c()
        ),
        undocCod = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "var",
            children = c(),
            title = "List of Undocumented Codes",
            description = "Values whose meaning is unknown.",
            examples = "<var><undocCod>Responses for categories 9 and 10 are unavailable.</undocCod></var>"
        ),
        unitType = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                numberOfUnits = list(
                    type = "xs:integer",
                    description = "Number of units in the sampling frame.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "frameUnit",
            children = c(),
            title = "Unit Type",
            description = "Describes the type of sampling frame unit using a conceptualText structure supporting a description and the use of an external controlled Vocabulary.",
            examples = "<unitType numberOfUnits=150000\"><concept vocab=\"SampleFrame_UnitType\">telephoneNumber</concept>Primary listed owners of published phone numbers in the City of St. Paul</unitType> "
        ),
        universe = list(
            optional = TRUE,
            recommended = TRUE,
            repeatable = TRUE,
            attributes = list(
                xmlang = list(
                    type = "xs:language",
                    description = "Language of the Mode of Collection CV item. ISO 639-1 codes are strongly encouraged to be used.",
                    values = c(),
                    default = c(),
                    optional = FALSE,
                    recommended = FALSE
                ),
                level = list(
                    type = "xs:string",
                    description = "Coding of the level to which universe applies, i.e., the study level, the file level (if different from study), the record group, the variable group, the nCube group, the variable, or the nCube level.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                clusion = list(
                    type = "xs:string",
                    description = "Groups included (I) in or excluded (E) from the universe.",
                    values = c("I", "E"),
                    default = "I",
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = c("nCube", "nCubeGrp", "sampleFrame", "sumDscr", "var", "varGrp"),
            children = c(),
            title = "Universe",
            description = "The group of persons or other elements that are the object of research and to which any analytic results refer. Age,nationality, and residence commonly help to delineate a given universe, but any of a number of factors may be involved, such as sex, race, income, veteran status, criminal convictions, etc. The universe may consist of elements other than persons, such as housing units, court cases, deaths, countries, etc. In general, it should be possible to tell from the description of the universe whether a given individual or element (hypothetical or real) is a member of the population under study. If all the variables/nCubes described in the data documentation relate to the same population, e.g., the same set of survey respondents, this element would be unnecessary at data description level. In this case, universe can be fully described at the study level. For forward-compatibility, DDI Lifecycle XHTML tags may be used in this element. This element may be repeated only to support multiple language expressions of the content.",
            examples = c(
                "<universe clusion=\"I\">Individuals 15-19 years of age. </universe>",
                "<universe clusion=\"E\">Individuals younger than 15 and older than 19 years of age.</universe>"
            )
        ),
        updateProcedure = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "sampleFrame",
            children = c(),
            title = "Instrument Development",
            description = "Description of how and with what frequency the sample frame is updated.",
            examples = "<updateProcedure>Changes are collected as they occur through registration and loss of phone number from the specified geographic area. Data are compiled for the date June 1st of odd numbered years, and published on July 1st for the following two-year period.</updateProcedure> "
        ),
        usage = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "controlledVocabUsed",
            children = c("selector", "specificElements", "attribute"),
            title = "Usage",
            description = "Defines where in the instance the controlled vocabulary which is identified is utilized. A controlled vocabulary may occur either in the content of an element or in an attribute on an element. The usage can either point to a collection of elements using an XPath via the selector element or point to a more specific collection of elements via their identifier using the specificElements element. If the controlled vocabulary occurs in an attribute within the element, the attribute element identifies the specific attribute. When specific elements are specified, an authorized code value may also be provided. If the current value of the element or attribute identified is not in the controlled vocabulary or is not identical to a code value, the authorized code value identifies a valid code value corresponding to the meaning of the content in the element or attribute.",
            examples = c()
        ),
        useStmt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("dataAccs", "metadataAccs", "sampleFrame"),
            children = c("confDec", "specPerm", "restrctn", "contact", "citReq", "deposReq", "conditions", "disclaimer"),
            title = "Use Statement",
            description = "Information on terms of use for the data collection. This element may be repeated only to support multiple language expressions of the content.",
            examples = c()
        ),
        validPeriod = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                event = list(
                    type = "xs:string",
                    description = "Type of event.",
                    values = c("start", "end", "single"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "sampleFrame",
            children = c(),
            title = "Valid Period",
            description = "Defines a time period for the validity of the sampling frame. Enter dates in YYYY-MM-DD format.",
            examples = "<validPeriod event=start\">2009-07-01</validPeriod><validPeriod event=\"end\">2011-06-30</validPeriod> "
        ),
        valrng = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access and Metadata Access section that describe access conditions for this range.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c("item", "range", "key", "notes"),
            title = "Range of Valid Data Values",
            description = "Values for a particular variable that represent legitimate responses.",
            examples = c(
                "<valrng><range min=\"1\" max=\"3\"/></valrng>",
                "<valrng><item VALUE=\"1\"/><item VALUE=\"2\"/><item VALUE=\"3\"/></valrng>"
            )
        ),
        var = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                name = list(
                    type = "xs:string",
                    description = "Contains the so-called \"short label\" for the variable, limited to eight characters in many statistical analysis systems such as SAS or SPSS.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                wgt = list(
                    type = "xs:string",
                    description = "Whether the variable is a weight.",
                    values = c("wgt", "not-wgt"),
                    default = "not-wgt",
                    optional = TRUE,
                    recommended = FALSE
                ),
                wgt_var = list(
                    type = "xs:IDREFS",
                    description = c("A reference to the variable(s) containing the weight used.", "(the actual attribute is named \"wgt-var\" in the final XML file.)"),
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                weight = list(
                    type = "xs:IDREFS",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                qstn = list(
                    type = "xs:IDREFS",
                    description = "Question ID for the variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                files = list(
                    type = "xs:IDREFS",
                    description = "ID of the file(s) to which the variable belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                vendor = list(
                    type = "xs:string",
                    description = "Origin of the proprietary format and includes SAS, SPSS, ANSI, and ISO.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                dcml = list(
                    type = "xs:string",
                    description = "Number of decimal points in the variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                intrvl = list(
                    type = "xs:string",
                    description = "Interval type; options are discrete or continuous.",
                    values = c("contin", "discrete"),
                    default = "discrete",
                    optional = TRUE,
                    recommended = FALSE
                ),
                rectype = list(
                    type = "xs:string",
                    description = "ID of the record type to which the variable belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Summary data description references which record the ID values of all elements within the summary data description section of the Study Description which might apply to the variable. These elements include: time period covered, date of collection, nation or country, geographic coverage, geographic unit, unit of analysis, universe, and kind of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                methrefs = list(
                    type = "xs:IDREFS",
                    description = "Methodology and processing references which record the ID values of all elements within the study methodology and processing section of the Study Description which might apply to the variable. These elements include information on data collection and data appraisal (e.g., sampling, sources, weighting, data cleaning, response rates, and sampling error estimates).",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                pubrefs = list(
                    type = "xs:IDREFS",
                    description = "Link to publication/citation references and records the ID values of all citations elements within Other Study Description Materials or Other Study-Related Materials that pertain to this variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in the Data Access and Metadata Access section that describe access conditions for this variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                aggrMeth = list(
                    type = "xs:string",
                    description = "Type of aggregation method used, for example 'sum', 'average', 'count'. If a value of \"other\" is given a term from a controlled vocabulary should be used in the \"otherAggrMeth\" attribute.",
                    values = c("sum", "average", "count", "mode", "median", "maximum", "minimum", "percent", "other"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherAggrMeth = list(
                    type = "xs:NMTOKEN",
                    description = "A value from a controlled vocabulary when the aggrMeth attribute has a value of \"other\".This option should only be used when applying a controlled vocabulary to this attribute. Use the complex element controlledVocabUsed to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                measUnit = list(
                    type = "xs:string",
                    description = "Measurement unit, for example 'km', 'miles', etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                scale = list(
                    type = "xs:string",
                    description = "Unit of scale, for example 'x1', 'x1000', etc.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                origin = list(
                    type = "xs:string",
                    description = "Point of origin for anchored scales.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                nature = list(
                    type = "xs:string",
                    description = "Nature  (measurement level) of the variable.",
                    values = c("nominal", "ordinal", "interval", "ratio", "percent", "other"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherNature = list(
                    type = "xs:string",
                    description = "A value from a controlled vocabulary. Use the complex element controlledVocabUsed to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                additivity = list(
                    type = "xs:string",
                    description = "Type of additivity",
                    values = c("stock", "flow", "non-additive", "other"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherAdditivity = list(
                    type = "xs:NMTOKEN",
                    description = "A value from a controlled vocabulary. Use the complex element controlledVocabUsed to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                temporal = list(
                    type = "xs:string",
                    description = "Whether the variable relays time-related information.",
                    values = c("Y", "N"),
                    default = "N",
                    optional = TRUE,
                    recommended = FALSE
                ),
                geog = list(
                    type = "xs:string",
                    description = "Whether the variable relays geographic information.",
                    values = c("Y", "N"),
                    default = "N",
                    optional = TRUE,
                    recommended = FALSE
                ),
                geoVocab = list(
                    type = "xs:string",
                    description = "Coding scheme used in the variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                catQnty = list(
                    type = "xs:string",
                    description = "Number of categories found in the variable, and is used primarily for aggregate data files for verifying cell counts in nCubes.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                representationType = list(
                    type = "xs:string",
                    description = "Captures the specific DDI Lifecycle representation type to facilitate translation between DDI 2 and DDI Lifecycle. If the \"other\" value is used, a term from a controlled vocabulary may be supplied in the otherRepresentationType attribute.",
                    values = c("text", "numeric", "code", "datetime", "geographicLocationCode", "geographicStructureCode", "scale", "other"),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherRepresentationType = list(
                    type = "xs:NMTOKEN",
                    description = "A value from a controlled vocabulary. Use the complex element controlledVocabUsed to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataDscr",
            children = c("location", "labl", "imputation", "security", "respUnit", "anlysUnit", "qstn", "valrng", "invalrng", "undocCod", "universe", "TotlResp", "sumStat", "txt", "stdCatgry", "catgryGrp", "catgry", "codInstr", "verStmt", "concept", "derivation", "varFormat", "geoMap", "catLevel", "notes"),
            title = "Variable",
            description = "This element describes all of the features of a single variable in a social science data file. The following elements are repeatable to support multi-language content: anlysUnit, embargo, imputation, respUnit, security, TotlResp.",
            examples = c()
        ),
        varFormat = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "Indicates if the variable is character or numeric.",
                    values = c("character", "numeric"),
                    default = "numeric",
                    optional = TRUE,
                    recommended = FALSE
                ),
                formatname = list(
                    type = "xs:string",
                    description = "In some cases, it may provide the name of the particular, proprietary format actually used.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                schema = list(
                    type = "xs:string",
                    description = "Identifies the vendor or standards body that defined the format.",
                    values = c("SAS", "SPSS", "IBM", "ANSI", "ISO", "XML-Data", "other"),
                    default = "ISO",
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherSchema = list(
                    type = "xs:NMTOKEN",
                    description = "A value from a controlled vocabulary. The complex element controlledVocabUsed should be used to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                category = list(
                    type = "xs:string",
                    description = "Describes what kind of data the format represents.",
                    values = c("date", "time", "currency", "other"),
                    default = "other",
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherCategory = list(
                    type = "xs:NMTOKEN",
                    description = "A value from a controlled vocabulary. The complex element controlledVocabUsed should be used to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                URI = list(
                    type = "xs:string",
                    description = "A network identifier for the format definition.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "var",
            children = c(),
            title = "Variable Format",
            description = "The technical format of the variable in question.",
            examples = c(
                "<var><varFormat type=\"numeric\" schema=\"SAS\" formatname=\"DATE\" category=\"date\">The number in this variable is stored in the form 'ddmmmyy' in SAS format.</varFormat></var>",
                "<var><varFormat type=\"numeric\" formatname=\"date.iso8601\" schema=\"XML-Data\" category=\"date\" URI=\"http://www.w3.org/TR/1998/NOTE-XML-data/\">19541022</varFormat></var>"
            )
        ),
        varGrp = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                type = list(
                    type = "xs:string",
                    description = "General type of grouping of the variables (Details in the description section).",
                    values = c("section", "multipleResp", "grid", "display", "repetition", "subject", "version", "iteration", "analysis", "pragmatic", "record", "file", "randomized", "other"),
                    default = "other",
                    optional = TRUE,
                    recommended = FALSE
                ),
                otherType = list(
                    type = "xs:NMTOKEN",
                    description = "A value from a controlled vocabulary. The complex element controlledVocabUsed should be used to identify the controlled vocabulary to which the selected term belongs.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                var = list(
                    type = "xs:IDREFS",
                    description = "IDs of all constituent variables.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                varGrp = list(
                    type = "xs:IDREFS",
                    description = "Reference to all subsidiary variable groups which nest underneath the current varGrp. This allows for encoding of a hierarchical structure of variable groups.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                name = list(
                    type = "xs:string",
                    description = "A name, or short label, for the group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                sdatrefs = list(
                    type = "xs:IDREFS",
                    description = "Summary data description references that record the ID values of all elements within the summary data description section of the Study Description that might apply to the group. These elements include: time period covered, date of collection, nation or country, geographic coverage, geographic unit, unit of analysis, universe, and kind of data.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                methrefs = list(
                    type = "xs:IDREFS",
                    description = "Methodology and processing references which record the ID values of all elements within the study methodology and processing section of the Study Description which might apply to the group. These elements include information on data collection and data appraisal (e.g., sampling, sources, weighting, data cleaning, response rates, and sampling error estimates).",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                pubrefs = list(
                    type = "xs:IDREFS",
                    description = "Link to publication/citation references and records the ID values of all citations elements within codeBook/stdyDscr/othrStdyMat or codeBook/otherMat that pertain to this variable group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                access = list(
                    type = "xs:IDREFS",
                    description = "ID values of all elements in codeBook/stdyDscr/dataAccs or codeBook/stdyDscr/metadataAccs of the document that describe access conditions for this variable group.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                nCube = list(
                    type = "xs:string",
                    description = "Included in 2.0 and subsequent versions in ERROR. DO NOT USE THIS ATTRIBUTE. It is retained only for purposes of backward-compatibility. DEPRECATED.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "dataDscr",
            children = c("labl", "txt", "concept", "defntn", "universe", "notes"),
            title = "Variable Group",
            description = c(
                "A group of variables that may share a common subject, arise from the interpretation of a single question, or are linked by some other factor.",
                "Variable groups are created this way in order to permit variables to belong to multiple groups, including multiple subject groups such as a group of variables on sex and income, or to a subject and a multiple response group, without causing overlapping groups. Variables that are linked by use of the same question need not be identified by a Variable Group element because they are linked by a common unique question identifier in the Variable element. Note that as a result of the strict sequencing required by XML, all Variable Groups must be marked up before the Variable element is opened. That is, the mark-up author cannot mark up a Variable Group, then mark up its constituent variables, then mark up another Variable Group.",
                "- section: Questions which derive from the same section of the questionnaire, e.g., all variables located in Section C.",
                "- multiple response: Questions where the respondent has the opportunity to select more than one answer from a variety of choices, e.g., what newspapers have you read in the past month (with the respondent able to select up to five choices).",
                "- grid: Sub-questions of an introductory or main question but which do not constitute a multiple response group, e.g., I am going to read you some events in the news lately and you tell me for each one whether you are very interested in the event, fairly interested in the fact, or not interested in the event.",
                "- display: Questions which appear on the same interview screen (CAI) together or are presented to the interviewer or respondent as a group.",
                "- repetition: The same variable (or group of variables) which are repeated for different groups of respondents or for the same respondent at a different time.",
                "- subject: Questions which address a common topic or subject, e.g., income, poverty, children.",
                "- version: Variables, often appearing in pairs, which represent different aspects of the same question, e.g., pairs of variables (or groups) which are adjusted/unadjusted for inflation or season or whatever, pairs of variables with/without missing data imputed, and versions of the same basic question.",
                "- iteration: Questions that appear in different sections of the data file measuring a common subject in different ways, e.g., a set of variables which report the progression of respondent income over the life course.",
                "- analysis: Variables combined into the same index, e.g., the components of a calculation, such as the numerator and the denominator of an economic statistic.",
                "- pragmatic: A variable group without shared properties.",
                "- record: Variable from a single record in a hierarchical file.",
                "- file: Variable from a single file in a multifile study.",
                "- randomized: Variables generated by CAI surveys produced by one or more random number variables together with a response variable, e.g, random variable X which could equal 1 or 2 (at random) which in turn would control whether Q.23 is worded \"men\" or \"women\", e.g., would you favor helping [men/women] laid off from a factory obtain training for a new job?",
                "- other: Variables which do not fit easily into any of the categories listed above, e.g., a group of variables whose documentation is in another language."
            ),
            examples = c()
        ),
        varQnty = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = c("dimensns", "recDimnsn"),
            children = c(),
            title = "Overall Variable Count",
            description = "Number of variables.",
            examples = "<varQnty>27</varQnty>"
        ),
        varRange = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                start = list(
                    type = "xs:IDREF",
                    description = "ID of the first variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                end = list(
                    type = "xs:IDREF",
                    description = "ID of the last variable.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "derivation",
            children = c(),
            title = "Variable Range",
            description = "Reference to the range of variables used by the derivation.",
            examples = "<varRange start=\"V1\" end=\"V3\"/>"
        ),
        verResp = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                affiliation = list(
                    type = "xs:string",
                    description = "Institutional affiliation.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                personalID = list(
                    type = "xs:string",
                    description = "Unique identifier of the organization / person.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                typeOfPersonalID = list(
                    type = "xs:string",
                    description = "Type of unique identifier.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "verStmt",
            children = c(),
            title = "Version Responsibility Statement",
            description = "The organization or person responsible for the version of the work. If the attribute \"personalID\" is used, \"typeOfPersonalID\" should also be provided.",
            examples = c(
                "<verResp>Zentralarchiv fuer Empirische Sozialforschung</verResp>",
                "<verResp>Inter-university Consortium for Political and Social Research</verResp>",
                "<var><verStmt><verResp>Zentralarchiv fuer Empirische Sozialforschung</verResp></verStmt></var>",
                "<nCube><verStmt><verResp>Zentralarchiv fuer Empirische Sozialforschung</verResp></verStmt></nCube>"
            )
        ),
        version = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(
                date = list(
                    type = "xs:string",
                    description = "ISO standard for dates (YYYY-MM-DD) is recommended.",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                ),
                type = list(
                    type = "xs:string",
                    description = "",
                    values = c(),
                    default = c(),
                    optional = TRUE,
                    recommended = FALSE
                )
            ),
            parents = "verStmt",
            children = c(),
            title = "Version",
            description = "Also known as release or edition. If there have been substantive changes in the data/documentation since their creation, this statement should be used at the appropriate level.",
            examples = c(
                "<version type=\"edition\" date=\"1999-01-25\">Second ICPSR Edition</version>",
                "<var><verStmt><version type=\"version\" date=\"1999-01-25\">Second version of V25</version></verStmt></var>",
                "<nCube><verStmt><version type=\"version\" date=\"1999-01-25\">Second version of N25</version></verStmt></nCube>"
            )
        ),
        verStmt = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = c("citation", "docSrc", "fileCitation", "fileTxt", "nCube", "sourceCitation", "var"),
            children = c("version", "verResp", "notes"),
            title = "Version Statement",
            description = "Version statement for the work at the appropriate level: marked-up document; marked-up document source; study; study description, other material; other material for study. A version statement may also be included for a data file, a variable, or an nCube.",
            examples = "<verStmt><version type=\"version\" date=\"1999-01-25\">Second version</version></verStmt>"
        ),
        weight = list(
            optional = TRUE,
            recommended = FALSE,
            repeatable = TRUE,
            attributes = list(),
            parents = "dataColl",
            children = c("concept", "txt"),
            title = "Weighting",
            description = "The use of sampling procedures may make it necessary to apply weights to produce accurate statistical results. Describe here the criteria for using weights in analysis of a collection. If a weighting formula or coefficient was developed, provide this formula, define its elements, and indicate how the formula is applied to data. ",
            examples = c(
                "<weight>The 1996 NES dataset includes two final person-level analysis weights which incorporate sampling, nonresponse, and post-stratification factors. One weight (variable #4) is for longitudinal micro-level analysis using the 1996 NES Panel. The other weight (variable #3) is for analysis of the 1996 NES combined sample (Panel component cases plus Cross-section supplement cases). In addition, a Time Series Weight (variable #5) which corrects for Panel attrition was constructed. This weight should be used in analyses which compare the 1996 NES to earlier unweighted National Election Study data collections.</weight>",
                "<weight><concept>PropensityWeighting</concept>The weight was determined using the online opt-in sample, the entire population of a synthetic dataset, and a statistical model to estimate the probability of a case occuring in either the sythetic or opt-in dataset.</weight>"
            )
        ),
        westBL = list(
            optional = FALSE,
            recommended = FALSE,
            repeatable = FALSE,
            attributes = list(),
            parents = "geoBndBox",
            children = c(),
            title = "West Bounding Longitude",
            description = "The westernmost coordinate delimiting the geographic extent of the dataset. A valid range of values, expressed in decimal degrees (positive east and positive north), is: -180,0 <=West Bounding Longitude Value <= 180,0",
            examples = "<westBL>4.789583</westBL>"
        )
    ),
    envir=cacheEnv
)





assign(
    "DDIC_global_attributes",
    list(
        ID = list(
            type = "xs:ID",
            description = "Anything that can uniquely identify the element within the DDI Codebook XML file",
            values = c(),
            default = c(),
            optional = TRUE,
            recommended = FALSE
        ),
        xmlang = list(
            type = "xs:language",
            description = c(
                "This attribute specifies the language used in the contents and attribute values of any element in the XML document. Use of ISO (www.iso.org) language codes is recommended.",
                "(the actual attribute is named \"xml:lang\" in the final XML file.)"
            ),
            values = c(),
            default = c(),
            optional = TRUE,
            recommended = FALSE
        ),
        source = list(
            type = "xs:string",
            description = "This attribute identifies the source that provided information in the element. For instance, if the documentation contains two differing sets of information on Sampling Procedure -- one provided by the data producer and one by the archive where the data is deposited -- this information can be distinguished through the use of the source attribute",
            values = c("archive", "producer"),
            default = "producer",
            optional = TRUE,
            recommended = FALSE
        ),
        elementVersion = list(
            type = "xs:string",
            description = "Captures version of the element",
            values = c(),
            default = c(),
            optional = TRUE,
            recommended = FALSE
        ),
        elementVersionDate = list(
            type = c(),
            description = "Indicates version date for the element. Use YYYY-MM-DD, YYYY-MM, or YYYY formats.",
            values = c(),
            default = c(),
            optional = TRUE,
            recommended = FALSE
        ),
        ddiLifecycleUrn = list(
            type = c(),
            description = "Used to capture the DDI-Lifecycle type URN for the element. This may be captured during translation from DDI-Lifecycle to DDI-Codebook structure or in preparation for transferring to a DDI-Lifecycle structure.",
            values = c(),
            default = c(),
            optional = TRUE,
            recommended = FALSE
        ),
        ddiCodebookUrn = list(
            type = c(),
            description = "Used to capture the DDI-Codebook type URN for the element. This is used to assign a DDI-Codebook specific URN to the element, according the format prescribed by the DDI-Codebook standard.",
            values = c(),
            default = c(),
            optional = TRUE,
            recommended = FALSE
        )
    ),
    envir=cacheEnv
)
