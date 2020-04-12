package covid_19

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl

import scala.math.{E, pow}

// STONP - Prediction Model for Mortality of COVID-19

object STONP {
    case class State(
        help:Boolean     = false,
        female:Boolean   = false,
        SpO2:Boolean     = false,
        ToC:Boolean      = false,
        NTproBNP:Boolean = false
    ) {
        lazy val beta_female:Double = if(!female) 0 else 2.575
        lazy val beta_spo2:Double = if(!SpO2) 0 else 4.250
        lazy val beta_toc:Double = if(!ToC) 0 else 4.388
        lazy val beta_ntprobnp:Double = if(!NTproBNP) 0 else 5.611
        lazy val logit = beta_female + beta_spo2 + beta_toc + beta_ntprobnp - 7.938

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
                case "help"     => $.modState(state => state.copy(help = !state.help))
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
                        <.h1(
                            <.span(^.className:="icon has-text-gray",
                                <.i(^.className:="fas fa-virus")
                            ),
                            " STONP--Prediction Model for In-hospital Death of Elderly COVID-19"
                        ),
                        <.p("(Apply to patients ≥75 Years of Age ONLY)")
                    ),
                    <.div(^.className:="columns is-multiline",
                        {
                            def title = <.div(^.className:="columns is-mobile is-gapless",
                                <.div(^.className:="column is-5",
                                    <.button(^.className:="button is-fullwidth is-light is-grey",
                                        ^.onClick ==> update("help"),
                                        <.span("Variables"),
                                        <.span(^.className:="icon is-small has-text-info",
                                            <.i(^.className:="fas fa-info-circle")
                                        )
                                    )
                                ),
                                if(!s.help) {
                                    Seq(
                                        <.div(^.className:="column is-4",
                                            <.button(^.className:="button is-fullwidth is-light is-grey", "Value")
                                        ),
                                        <.div(^.className:="column is-3",
                                            <.button(^.className:="button is-fullwidth is-light is-grey", "Beta")
                                        )
                                    ).toTagMod
                                } else <.div(^.className:="column")
                            )

                            Seq(
                                <.div(^.className:="column is-half", title),
                                <.div(^.className:="column is-half is-hidden-mobile  is-hidden-tablet-only", title),
                            ).toTagMod
                        },
                        Seq(
                            ("sex",         s.female,   s.beta_female, <.span("Sex"), "Female", "Male", "female/male."),
                            ("spo2",        s.SpO2,     s.beta_spo2, <.span("SpO", <.sub("2")), "≥ 90%", "< 90%", "within 1 hour before or after hospital admission."),
                            ("toc",         s.ToC,      s.beta_toc, <.span("Temp(°C/°F)"), "< 37.3/99.14", "≥ 37.3/99.14", "armpit temperature within 1 hour before or after hospital admission."),
                            ("ntprobnp",    s.NTproBNP, s.beta_ntprobnp, <.span("NT-proBNP (ng/L)"), "< 1800", "≥ 1800", "within 24 hour before or after hospital admission."),
                        ).map{case (name, condition, beta, head, ifFalse, ifTrue, help) => {
                            <.div(^.className:="column is-half",
                                <.div(^.className:="columns is-mobile is-gapless",
                                    if(s.help) {
                                        <.div(^.className:="column",
                                            <.span(^.className:="has-text-warning has-text-weight-bold", head),
                                            <.span(^.className:="has-text-grey", ": "),
                                            <.span(^.className:="has-text-info", help)
                                        )
                                    } else {
                                        Seq(
                                            <.div(^.className:="column is-5",
                                                <.button(^.className:="button is-fullwidth is-rounded is-white", ^.onClick ==> update(name), head)
                                            ),
                                            <.div(^.className:="column is-4",
                                                if(condition) {
                                                    <.button(^.className:="button is-fullwidth is-rounded is-danger", ^.onClick ==> update(name), ifTrue)
                                                } else {
                                                    <.button(^.className:="button is-fullwidth is-rounded is-primary", ^.onClick ==> update(name), ifFalse)
                                                }
                                            ),
                                            <.div(^.className:="column is-3",
                                                <.button(^.className:="button is-fullwidth is-rounded is-light", ^.onClick ==> update(name), beta)
                                            )
                                        ).toTagMod
                                    }
                                )
                            )
                        }}.toTagMod
                    ),
                    <.div(^.className:="level is-mobile",
                        <.div(^.className:="level-item has-text-centered",
                            <.div(
                                <.p(^.className:="heading", "Predicted in-hospital death rate"),
                                <.p(^.className:="title", f"${s.deathRate}%.1f%%")
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
                        <.p("Predicted in-hospital death rate = (e", <.sup("Logit"), ") / (1 + e", <.sup("Logit"), ")"),
                        <.p("Logit = Sum(Beta) - 7.938"),
                        <.p("All measurements taken on admission."),
                        <.ul(
                            <.li("SpO", <.sub("2"), ": without oxygen supply."),
                            <.li("Temp: axillary body temperature.")
                        )
                    )
                )
            )
        )
    }

    def apply() = {
        val C = ScalaComponent.builder[Unit]("STONP")
        .initialState(State())
        .renderBackend[Backend]
        .build

        C()
    }
}