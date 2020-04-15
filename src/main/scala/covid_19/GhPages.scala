package covid_19

import org.scalajs.dom.document

object GhPages {
    def main(args: Array[String]):Unit = {
        STONP.apply().renderIntoDOM( document.getElementById("page") )
    }
}