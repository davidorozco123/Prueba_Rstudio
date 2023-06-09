medios <- c("latinus_us", "Milenio", "proceso", "El_Universal_Mx", "guruchuirer",
            "QueImportaTV", "Reforma", "revistaetcetera", "politicomx", "SinEmbargoMX",
            "diario24horas", "heraldodemexico", "ElFinanciero_Mx", "mileniotv", "sopitas",
            "AztecaNoticias", "MVSNoticias", "Forbes_Mexico", "Excelsior", "Nacion321",
            "LaRazon_mx", "ImagenTVMex", "ExpPolitica", "reformanacional", "Reporte_Indigo",
            "eleconomista", "PublimetroMX", "lasillarota", "DiariodeYucatan", "sdpnoticias",
            "RuidoEnLaRed", "Pajaropolitico", "QueImportaTV", "emeequis", "PeriodicoZocalo",
            "lajornadaonline", "EjeCentral", "TabascoHOY", "AcustikInforma", "cafepolitico",
            "RedAMLOmx", "bsbcmundo", "N79news", "laoctavadigital", "TheEconomist",
            "DIARIOUNOMASUNO", "adn40", "LaCronicaDeHoy","marranoticias",
            "CNTAMAULIPAS", "PoliticaalDia", "ForoJuridico", "mipuntodevistaa", "olivanoticias",
            "OlaNoticias", "DDMexico", "Rlegislatura", "lado_mx", "afondoJAL",
            "LaBrechame", "Asi_Sucede", "FortunayPoder", "EnterateMexico_", "ElFinancieroTv",
            "LibreenelSur", "OnceNoticiasTV", "TelRojo", "hombresdelpoder", "Revolucion3_0",
            "cadenapolitica", "Estado20", "lavozfrontera", "LaSagaOficial", "FrentedeLibera3",
            "visionliberal", "enlabarra", "ContraReplicaMX", "LaJornadaBC", "guardian",
            "Reuters", "CENTROHALTUNG", "enfasisnoticias", "InsNacOtrosDato", "PorteroMexico",
            "cursorenla", "espectador_el", "MxNoticiasNet", "TiempoMonclova", "Efekto10",
            "tiempo_mx", "AgenciaTres", "telediario", "terrableu", "LaListanews",
            "WEEBlogy", "infobaemexico", "BuenasyMalasMx", "elpaismexico", "DiarioPalentino",
            "accionavecinos", "nytimes", "LiibertadYA", "RIDNoticias", "Ecodiariozac",
            "DeniuzDesarroll", "hola_atizapan", "ndl_noticias", "PCMNoticias", "MarcrixNoticias",
            "joveneslgbtmex", "ElSoldeHgo", "SoldeIrapuato", "ChayoterosQRoo", "Museo_Nacional",
            "NoticiasRTQ", "cunadegrillos", "QuestoyQueLotro", "Univ_Opinion", "vivavoznoticias",
            "OROSOLIDORedes", "heraldoags", "RadioBI", "ElSoldeTijuana1", "PausaMx",
            "EsDiarioPopular", "DiarioChiapas", "DiarioCoahuila", "PeriodicoZocalo", "ZocaloMVA",
            "torreon", "NoticieroColima", "ABarloventoInfo", "CapitalMX_", "QuadratinMexico",
            "RevistaZocalo", "elsolde_mexico", "Congresistas", "Foro_TV", "ElEstadoMx",
            "noticiascd_mx", "almomento_mx", "WRADIOMexico", "ElBigDataMx", "90gradosmx",
            "MetropoliHoy", "EnfoqueNoticias", "Radio_Formula", "DiarioPresente",
            "889Noticias", "MexicoEnTweets", "DiarioNoticiaI", "SinEmbargoTR", "CanalOnceTV",
            "reformacancha", "unomasunomx", "DenuncioDF", "oficialNTCD", "Forbes_Mexico",
            "xhunes", "BajoPalabraGro", "elsoldeAca", "ImagendeMexico", "soldleon",
            "SoldeSalamanca", "CapitalMexico", "CriterioHidalgo", "SintesisHgo", "informador_MEX",
            "Mugs_Noticias", "tolucanoticias", "michangoonga", "Quadratin_Mor", "_HomoPoliticus",
            "ADNInforma", "Codigo_Magenta", "Mty_Leones", "AsiEsMonterrey", "postamx",
            "elnortelocal", "ABCNoticiasMX", "PeriodistasOax", "TiempoDigitalMx", "adn_sureste",
            "e_consulta", "RealidadSieteMx", "AcustikInforma", "RealidadesPue", "observador1370",
            "IntoleranciaID", "e_MagazinePue", "DiarioPuntual", "NoticiasTribuna", "Politica_Puebla",
            "Quinceminutos", "elsoldepuebla1", "SUTERM92PUEBLA", "angulosiete", "RRNoticiasqro",
            "PlazaDeArmasQro", "TribunaQro", "CNqueretaro", "codice_informa", "noticiasdeqro",
            "Quadratin_Qtro", "SuresteSur", "lapancarta", "QuintaFuerzaMX", "lucesdelsiglo",
            "palabracaribe", "ELDEBATE", "Altavoznoticias", "ElHeraldoSLP",
            "Globalmediamx", "EosNoticiasSLP", "laorquestamx", "ProyectoPuente", "La_VerdadSonora",
            "mesa_plural", "Expresoweb", "paralosdeapie1", "AhoraTabasco", "TabascoHOY",
            "XEVATabasco", "LineaTabasco", "xevtfm", "HojasPoliticas", "lapalabratab",
            "rumbonuevo", "DiarioDeTabasco", "TVTenlinea", "noti13tabasco", "Notiviza",
            "Politica_Tlax", "CuartodeGuerraT", "alcalorpolitico", "billieparkernot", "xeunoticias",
            "Quadratin_Ver", "blogexpediente", "versionesmx", "imagendelgolfo", "DiariodeYucatan",
            "porticoomx", "soldezacatecas", "ZacatecasImagen", "InfoZacMx", "ntrzacatecas",
            "aficionzac", "VisioNoticiaZac", "PilotziNoticias", "AgenciaTelam", "TendenciasRSS", 
            "Cadenalatam", "JuventudMonreal", "TheWJP_mx", "TierraFertilMex",
            "HomoEspacios", "corpcomojeda", "RedMorenaOax", "dd_lascamaras", "RedMorenaMich",
            "mindmexico", "heyoaxacamx", "RedMorenaMex", "AgendaLGBTMx", "SintesisMexico",
            "teleSURtv", "Quien", "la_iguanatv", "OficialEnLaMira", "Imagen_Mx",
            "politicosmex","elwesomx", "ElInformanteMX", "MilenioTam",
            "EVTVMiami", "SergioyLupita", "elimparcialcom", "ElSoberanoMX",
            "contralinea", "24_morelos", "LaEncuesta_Mx", "nmas", "LaJornada", "elnorte",
            "el_Periodico", "quierotv_gdl", "Milenio","senadomexicano", "ExpansionMx",
            "CanalCongreso", "MorenaSenadores", "muralcom", "supercivicosmx", "vanguardiamx",
            "LideresMexicano", " RegeneracionMx", "AncopSports", "Proyecsaa", "UniversoCdmx",
            "AmbientalNewsMX", "rootslandmx", "gacetamc", "CANAL44TV", "nvinoticiasoax",
            "imagenZea", "telediariomty","CTCMedios", "enfoqueinforma", "StateDept",
            "ActualidadRT", "EFEnoticias", "pagina_siete", "eldiario", "USAenEspanol", "UCS_GCDMX",
            "corazn_abierto", "CFE_Contigo", "nexosmexico", "El_Ciudadano", "Merca20")
