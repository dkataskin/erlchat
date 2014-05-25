(function(){
    var loginForm = [{ view:"text", label:"Nickname", name:"nickname", id:"nickname" },
                     { view:"text", type:"password", label:"Password", name:"password", id:"password" },
                     { margin:5, cols:[
                         { view:"button", label:"Login", type:"form", click:"loginClick" },
                         { view:"button", label:"Cancel" }
                     ]}];

    webix.ui({
        type:"clean",
        rows:[
            getViewHeader(),
            { type:"clean", height:100 },
            { cols:[
                { type:"clean" },
                { view:"form", container:"login_form", width:300, elements:loginForm, id:"login_form" },
                { type:"clean" }
            ]}
        ]
    });

    loginClick = function (){
        console.log($$("login_form").getValues());

        $.post("/login", $$("login_form").getValues(), function(data){
                window.location.replace('/chat');
            });
        };
})();