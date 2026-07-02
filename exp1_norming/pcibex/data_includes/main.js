PennController.ResetPrefix(null); // Starts PennController

var showProgressBar = false;

SetCounter("counter", "inc", 1);

PennController.DebugOff(); 

Sequence("counter", "consent", "info", "horizontal", "explain1","explain2","explain3_1",
"explain3_2","explain4","explain5_1", "explain5_2", "explain6", "explain_final", "training",
"start", "exp_Block1", "break", "exp_Block2", SendResults(), "final") 

// Informed Consent Screen

newTrial ("consent",
    newHtml("consent", "consent.html")
        .css({"width": "90vw", "max-width": "50em"})  
        .center() 
        .print()
    ,
    
    newButton("Clique aqui para dar seu consentimento")
        .center() 
        .print()
        .wait()
);

// Personal Information Screen

let ageOptions = []
for (let i = 0; i<=100; i++) {
    ageOptions.push( String(i));
}

newTrial("info",

    newText("Por favor, antes de começarmos, preencha com os seus dados.<br>Os campos com * são obrigatórios. As informações só salvam ao final do experimento.")
        .css({"text-align": "center", "margin-bottom": "2em",
        "width": "90vw", "max-width": "600px"})
        .center()
        .print()
    ,

    newText("<p>Selecione abaixo a sua idade*:</p>").center().print(),
        newDropDown("inputAge", "selecionar")
            .add(...ageOptions)
            .center() 
            .print()
    ,

    newText("<p>Selecione seu gênero*:</p>").center().print(),
        newDropDown("inputGender", "selecionar")
            .add("Feminino", "Masculino", "Outro")
            .center() 
            .print()
    ,

    newText("<p>Selecione seu nível de escolaridade*:</p>").center().print(),
        newDropDown("inputSchool", "selecionar")
            .add(
                "Ensino Fundamental Incompleto",
                "Ensino Fundamental Completo",
                "Ensino Médio Incompleto",
                "Ensino Médio Completo",
                "Ensino Superior Incompleto",
                "Ensino Superior Completo"
                )
            .center()
            .print()
    ,

    newText("<p>Digite abaixo a cidade onde você vive*:</p>").center().print(),
        newTextInput("inputLocation", "")
            .center()
            .print()
    ,

    newText("<p>Digite abaixo seu email*:</p>")
        .center()
        .print()
    ,
    
    newTextInput("inputEmail", "")
        .center()
        .print()
    ,

    newText("<p>Se quiser receber uma declaração ACC, coloque seu nome:</p>").center().print(),
        newTextInput("inputName", "")
            .center()
            .print()
    ,

    newText("<p>Você tem familiaridade com emojis / stickers / gifs*?</p>").center().print(),
        newDropDown("inputEmoji", "selecionar")
            .add(
                "Uso e recebo diariamente.",
                "Uso e recebo algumas vezes na semana.",
                "Uso e recebo de vez em quando.",
                "Uso raramente ou nunca, mas recebo diariamente",
                "Uso raramente ou nunca, mas recebo algumas vezes na semana",
                "Uso raramente ou nunca, mas recebo de vez em quando",
                "Nunca recebo ou uso."
                )
            .center()
            .print()
    ,
    
    newText("<p> Se estiver usando o celular, <br>ative o modo paisagem e coloque-o na horizontal antes de prosseguir.</p>")
        .css({"margin-top": "2em", "text-align": "center"})
        .center()
        .print()
    ,
    
    newText("msgErro", "Por favor, preencha todos os campos obrigatórios antes de continuar")
        .color("red")
        .css("margin-bottom", "1em")
        .center()
        .print()
        .hidden()
        ,
        
    newVar("age").global(),
    newVar("gender").global(),
    newVar("school").global(),
    newVar("location").global(),
    newVar("email").global(),
    newVar("name").global(),
    newVar("famEmoji").global(),

    newButton("continuar", "Continuar (ativa modo Tela Cheia)")
        .css("margin-bottom", "2em")
        .center()
        .print()
        .wait(
            getDropDown("inputAge").test.selected()
                .and(getDropDown("inputGender").test.selected())
                .and(getDropDown("inputSchool").test.selected())
                .and(getTextInput("inputLocation").testNot.text(""))
                .and(getTextInput("inputEmail").testNot.text(""))
                .and(getDropDown("inputEmoji").test.selected())
                .failure(getText("msgErro").visible())
                .success(
                    getVar("age").set(getDropDown("inputAge")),
                    getVar("gender").set(getDropDown("inputGender")),
                    getVar("school").set(getDropDown("inputSchool")),
                    getVar("location").set(getTextInput("inputLocation")),
                    getVar("email").set(getTextInput("inputEmail")),
                    getVar("name").set(getTextInput("inputName")),
                    getVar("famEmoji").set(getDropDown("inputEmoji")),
                    fullscreen()
                )
        )
)
.log("age", getVar("age"))
.log("gender", getVar("gender"))
.log("school", getVar("school"))
.log("location", getVar("location"))
.log("email", getVar("email"))
.log("name", getVar("name"))
.log("famEmoji", getVar("famEmoji"));

// Recommendation of putting the cellphone in horizontal position

newTrial("horizontal",
    newHtml("horizontal", "horizontal.html")
        .css({"width": "90vw", "max-width": "50em"})
        .center()
        .print()
    ,

    newButton("continue", "Continuar")
        .center()
        .print()
        .wait()
);

// Explanation

Template("explanation.csv" , row =>
    newTrial( row.label ,
        newHtml( row.htmlFile , row.htmlFile )
        .css({"width": "90vw", "max-width": "40em", "text-align": "left"})        
        .center()
        .print()
        ,
( row.backTarget == "NA" ?
            null 
            :
            newButton("comeback","Voltar")
                .callback( jump(row.backTarget) , end() )
                .css("margin-bottom", "1em")
                .center()
                .print()
        )
        ,
        newButton("continue", row.continueText )
            .center() 
            .print()
            .wait()
    )
);

// Scales function

let createTrialElements = (row) => [
    
    newImage("Sent", row.Sent)
        .print()
    ,
    
    // Question 1
    newText("question1", "<b>Em que grau o <i>emoji</i> é visualmente identificável?")
        .css({"margin-top": "1em",
        "margin-bottom": "1em"})
        .center()
        .print()
    ,
    
    // Scale 1
    newScale("likert1", "1", "2", "3", "4", "5", "6", "7")
        .label(0, newText("1<br>Impossível <br>de identificar").css("text-align", "center"))
        .label(6, newText("7<br>Perfeitamente <br>identificável").css("text-align", "center"))
        .labelsPosition("bottom")
        .center()
        .print()
        .wait()
        .log()
    ,
    
    newTimer("feedbackPause1", 500)
        .start()
        .wait()
    ,

    getText("question1").remove(),
    getScale("likert1").remove(),

    // Question 2
    newText("question2", "<b>Em que grau o <i>emoji</i> e a frase fazem sentido juntos?</b>")
        .css({"margin-top": "1em",
        "margin-bottom": "1em"})
        .center()
        .print()
    ,
        
    // Scale 2
    newScale("likert2", "1", "2", "3", "4", "5", "6", "7")
        .label(0, newText("1<br>Não fazem<br>sentido algum").css("text-align", "center"))
        .label(6, newText("7<br>Fazem sentido<br>perfeitamente").css("text-align", "center"))
        .labelsPosition("bottom")
        .center()
        .print()
        .wait()
        .log()
    ,
    
    newTimer("feedbackPause2", 500)
        .start()
        .wait()
    ,

    getText("question2").remove(),
    getScale("likert2").remove(),

    // Question 3
    newText("question3", "<b>Em que grau o <i>emoji</i> adiciona informação à frase?</b>")
        .css({"margin-top": "1em",
        "margin-bottom": "1em"})
        .center()
        .print()
    ,
    
    // Scale 3
    newScale("likert3", "1", "2", "3", "4", "5", "6", "7")
        .label(0, newText("1<br>Não adiciona<br>informação alguma").css({"text-align": "center"}))
        .label(6, newText("7<br>Adiciona muita<br>informação").css("text-align", "center"))
        .labelsPosition("bottom")
        .center()
        .print()
        .wait()
        .log()
    ,

    newTimer("timeout", 750).start().wait()
];

// Training rounds

Template("training.csv", row =>
    newTrial("training", ...createTrialElements(row) )
);

// Final screen before the start

newTrial("start",
    
   newHtml("start","start.html" )
        .css({"width": "90vw", "max-width": "30em", "text-align": "left"})       .center()
        .print()
   ,

    newButton("Clique aqui para iniciar.")
        .center()
        .print()
        .wait()
        
);

// Experiment

Template(GetTable("List.csv").setGroupColumn("Group"),
    row => newTrial("exp_Block" + row.Block, ...createTrialElements(row) )
   
   .log("Group", row.Group) 
   .log("Block", row.Block) 
   .log("Id", row.Id)
   .log("Cond", row.Cond)
   .log("Sent", row.Sent)
);

// Pauses

newTrial("break",
    newText("Você completou metade do experimento!<br><br>Faça uma pausa se desejar e clique em continuar para prosseguir.")
    .css({"margin-bottom": "2em","text-align": "center", "width": "90vw", "max-width": "600px"})      
    .center()
    .print()
    ,
    
    newButton("Continuar")
        .center()
        .print()
        .wait()
);
    
// Final screen ("thank you for participating")

newTrial("final",
    newText("<p> Obrigado pela participação! Suas respostas foram salvas.</p>")
        .center()
        .print()
    ,

    newText('<p> Aperte "Finalizar" para sair.</p>')
        .center()
        .css("margin-bottom", "1em")
        .print()
    ,

    newButton("Finalizar")
        .center()
        .print()
        .wait()
    ,
    exitFullscreen()
);