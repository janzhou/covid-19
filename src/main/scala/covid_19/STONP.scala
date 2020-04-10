package covid_19

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl

import scala.math.{E, pow}

// STONP - Prediction Model for Mortality of COVID-19

object STONP {
    case class State(
        female:Boolean   = false,
        SpO2:Boolean     = false,
        ToC:Boolean      = false,
        NTproBNP:Boolean = false
    ) {
        lazy val beta_female:Double = if(!female) 0 else 2.575
        lazy val beta_spo2:Double = if(!SpO2) 0 else 4.250
        lazy val beta_toc:Double = if(!ToC) 0 else 4.388
        lazy val beta_ntprobnp:Double = if(!NTproBNP) 0 else 5.611
        lazy val logit = beta_female + beta_spo2 + beta_toc + beta_ntprobnp - 8.886

        lazy val deathRate = {
            val p = pow(E, logit)
            val v = 100 * p / (1 + p)
            if(v > 100) {
                100.0
            } else if(v < 0) {
                0.0
            } else v
        }
    }

    class Backend($: BackendScope[Unit, State]) {

        def update(typ:String)(e:ReactEvent):Callback = {
            typ match {
                case "sex"      => $.modState(state => state.copy(female = !state.female))
                case "spo2"     => $.modState(state => state.copy(SpO2 = !state.SpO2))
                case "toc"      => $.modState(state => state.copy(ToC = !state.ToC))
                case "ntprobnp" => $.modState(state => state.copy(NTproBNP = !state.NTproBNP))
            }
        }

    
        def render(s:State): VdomElement = <.div(
            <.div(^.className:="section",
                <.div(^.className:="container",
                    <.div(^.className:="content",
                        <.h1("STONP—Prediction Model for Mortality of COVID-19"),
                        <.p("(Scoring Systems for Patients >= 75 Years of Age with 2019-Coronavirus Infected Disease)")
                    ),
                    <.div(^.className:="columns is-multiline",
                        <.div(^.className:="column is-half",
                            <.div(^.className:="columns is-mobile is-gapless",
                                <.div(^.className:="column"),
                                <.div(^.className:="column is-one-quarter",
                                    <.button(^.className:="button is-fullwidth is-rounded is-white", "beta")
                                )
                            )
                        ),
                        <.div(^.className:="column is-half is-hidden-mobile  is-hidden-tablet-only",
                            <.div(^.className:="columns is-mobile is-gapless",
                                <.div(^.className:="column"),
                                <.div(^.className:="column is-one-quarter",
                                    <.button(^.className:="button is-fullwidth is-rounded is-white", "beta")
                                )
                            )
                        ),
                        Seq(
                            ("sex",         s.female,   s.beta_female, "Sex", "Female", "Male"),
                            ("spo2",        s.SpO2,     s.beta_spo2, "SpO2", ">= 90%", "< 90%"),
                            ("toc",         s.ToC,      s.beta_toc, "T^oC", "< 37.3", ">= 37.3"),
                            ("ntprobnp",    s.NTproBNP, s.beta_ntprobnp, "NT-proBNP (ng/L)", "< 1800", ">= 1800"),
                        ).map{case (name, condition, beta, head, ifFalse, ifTrue) => {
                            <.div(^.className:="column is-half",
                                <.div(^.className:="columns is-mobile is-gapless",
                                    <.div(^.className:="column is-half",
                                        <.button(^.className:="button is-fullwidth is-rounded is-white", ^.onClick ==> update(name), head)
                                    ),
                                    <.div(^.className:="column is-one-quarter",
                                        if(condition) {
                                            <.button(^.className:="button is-fullwidth is-rounded is-danger", ^.onClick ==> update(name), ifTrue)
                                        } else {
                                            <.button(^.className:="button is-fullwidth is-rounded is-primary", ^.onClick ==> update(name), ifFalse)
                                        }
                                    ),
                                    <.div(^.className:="column is-one-quarter",
                                        <.button(^.className:="button is-fullwidth is-rounded is-light", ^.onClick ==> update(name), beta)
                                    ),
                                )
                            )
                        }}.toTagMod
                    ),
                    <.div(^.className:="level is-mobile",
                        <.div(^.className:="level-item has-text-centered",
                            <.div(
                                <.p(^.className:="heading", "Predicted Death Rate"),
                                <.p(^.className:="title", f"${s.deathRate}%.2f%%")
                            )
                        ),
                        <.div(^.className:="level-item has-text-centered",
                            <.div(
                                <.p(^.className:="heading", "Logit"),
                                <.p(^.className:="title", f"${s.logit}%0.4f")
                            )
                        )
                    ),
                    <.hr(),
                    <.div(^.className:="content",
                        <.p("Logit = Sum(beta) - 8.886"),
                        <.p("Predicted Death Rate = (e^Logit) / (1 + e^Logit)")
                    )
                )
            )
        )
    }

    def apply() = {
        val C = ScalaComponent.builder[Unit]("Stonp")
        .initialState(State())
        .renderBackend[Backend]
        .build

        C()
    }
}