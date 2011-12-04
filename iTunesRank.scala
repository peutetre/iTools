import java.net.{URLConnection, URL}
import scala.xml._
import scala.Console._

object iTunesRank {

    def main(args:Array[String]) = {

        if (args.length != 1) {
            println("Usage: iTunesRank appId")
        }
        else {
            val output = RED + "%s" + RESET + 
                         " place for app " +  YELLOW + 
                         "\"%s\"" +  RESET + 
                         " on the " +
                         RED + "%s" + RESET + " store for the category " +
                         MAGENTA + "#%s" + RESET
            searchRank(args(0)).map { all =>
                all.map { elt =>
                    println
                    println(output.format(elt._1, elt._5, elt._3, elt._4))
                }
            }
        }
    }

    def searchRank(appId:String) = {
        val urlDef = "http://itunes.apple.com/%s/rss/%s/limit=300/xml";

        val feedTypes = Map(
            "topfreeapplications" -> "Top Free Apps",
            "toppaidapplications" -> "Top Paid Apps",
            "topgrossingapplications" -> "Top Grossing Apps",
            "topfreeipadapplications" -> "Top Free iPad Apps",
            "toppaidipadapplications" -> "Top Paid iPad Apps",
            "topgrossingipadapplications" -> "Top Grossing iPad Apps",
            "newapplications" -> "New Apps",
            "newfreeapplications" -> "New Free Applications",
            "newpaidapplications" -> "New Paid Applications"
        )

        val countries = Map(
            "DZ" -> "Algeria", "AO" -> "Angola", "AI" -> "Anguilla",
            "AG" -> "Antigua and Barbuda", "AR" -> "Argentina", "AM" -> "Armenia",
            "AU" -> "Australia", "AT" -> "Austria", "AZ" -> "Azerbaijan",
            "BS" -> "Bahamas", "BH" -> "Bahrain", "BB" -> "Barbados",
            "BY" -> "Belarus", "BE" -> "Belgium", "BZ" -> "Belize",
            "BM" -> "Bermuda", "BO" -> "Bolivia", "BW" -> "Botswana",
            "BR" -> "Brazil", "VG" -> "British Virgin Islands", "BN" -> "Brunei Darussalam",
            "BG" -> "Bulgaria", "CA" -> "Canada", "KY" -> "Cayman Islands",
            "CL" -> "Chile", "CN" -> "China", "CO" -> "Colombia",
            "CR" -> "Costa Rica", "HR" -> "Croatia", "CY" -> "Cyprus",
            "CZ" -> "Czech Republic", "DK" -> "Denmark", "DM" -> "Dominica",
            "DO" -> "Dominican Rep.", "EC" -> "Ecuador", "EG" -> "Egypt",
            "SV" -> "El Salvador", "EE" -> "Estonia", "FI" -> "Finland",
            "FR" -> "France", "DE" -> "Germany", "GH" -> "Ghana",
            "GR" -> "Greece", "GD" -> "Grenada", "GT" -> "Guatemala",
            "GY" -> "Guyana", "HN" -> "Honduras", "HK" -> "Hong Kong",
            "HU" -> "Hungary", "IS" -> "Iceland", "IN" -> "India",
            "ID" -> "Indonesia", "IE" -> "Ireland", "IL" -> "Israel",
            "IT" -> "Italy", "JM" -> "Jamaica", "JP" -> "Japan",
            "JO" -> "Jordan", "KZ" -> "Kazakstan", "KE" -> "Kenya",
            "KR" -> "Korea, Republic Of", "KW" -> "Kuwait", "LV" -> "Latvia",
            "LB" -> "Lebanon", "LT" -> "Lithuania", "LU" -> "Luxembourg",
            "MO" -> "Macau", "MK" -> "Macedonia","MG" -> "Madagascar",
            "MY" -> "Malaysia", "ML" -> "Mali", "MT" -> "Malta",
            "MU" -> "Mauritius", "MX" -> "Mexico", "MD" -> "Moldova",
            "MS" -> "Montserrat", "NL" -> "Netherlands", "NZ" -> "New Zealand",
            "NI" -> "Nicaragua", "NE" -> "Niger", "NG" -> "Nigeria",
            "NO" -> "Norway", "OM" -> "Oman", "PK" -> "Pakistan",
            "PA" -> "Panama", "PY" -> "Paraguay", "PE" -> "Peru",
            "PH" -> "Philippines", "PL" -> "Poland", "PT" -> "Portugal",
            "QA" -> "Qatar", "RO" -> "Romania", "RU" -> "Russia",
            "SA" -> "Saudi Arabia", "SN" -> "Senegal", "SG" -> "Singapore",
            "SK" -> "Slovakia", "SI" -> "Slovenia", "ZA" -> "South Africa",
            "ES" -> "Spain", "LK" -> "Sri Lanka", "KN" -> "St. Kitts and Nevis",
            "LC" -> "St. Lucia", "VC" -> "St. Vincent and The Grenadines", "SR" -> "Suriname",
            "SE" -> "Sweden", "CH" -> "Switzerland", "TW" -> "Taiwan",
            "TZ" -> "Tanzania", "TH" -> "Thailand", "TT" -> "Trinidad and Tobago",
            "TN" -> "Tunisia", "TR" -> "Turkey", "TC" -> "Turks and Caicos Islands",
            "UG" -> "Uganda", "GB" -> "United Kingdom", "AE" -> "United Arab Emirates",
            "UY" -> "Uruguay", "US" -> "United States", "UZ" -> "Uzbekistan",
            "VE" -> "Venezuela", "VN" -> "Vietnam", "YE" -> "Yemen"
        )

        val idRegex = """.*id(\d*)?.*""".r

        countries.map { country =>
            print("%s...".format(country._1))
            feedTypes.map { t =>
                try {
                    val url = new URL(urlDef.format(country._1, t._1))
                    val conn = url.openConnection
                    val content = XML.load(conn.getInputStream)

                    (content \\ "entry").zip(1 to 300 ).flatMap( rslt => {
                        val (entry, i) = rslt
                        val title = (entry \ "title").text
                        (entry \ "id").flatMap(id => id.text match {
                            case idRegex(id) if id == appId => List((i, id, country._2, t._2, title))
                            case _ => Nil
                        })
                    })
                } catch {
                    case e => {
                        println("Error in %s for \"%s\": ".format(country._2, t._2) + e.getMessage)
                        Nil
                    }
                }
            }
        }.flatten.filter{ el => el.length > 0}
    }
}
