(function(){
    var loginForm = [{ view:"text", label:"Nickname", name:"nickname", id:"nickname" },
                     { view:"text", type:"password", label:"Password", name:"password", id:"password" },
                     { margin:5, cols:[
                         { view:"button", label:"Login", type:"form", click:"loginClick" },
                         { view:"button", label:"Cancel" }
                     ]}];

    webix.ui({
        rows:[
            {type: "header", template: "erlchat sample"},
            {container:"login_div",	view:"form", scroll:false, width:300, elements: loginForm, id:"login_form"}
        ]
    });

    loginClick = function (){
        console.log($$("login_form").getValues());

        $.post("/login", $$("login_form").getValues());
        };
})();