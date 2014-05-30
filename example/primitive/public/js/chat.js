(function(){
    webix.ui({ type:"clean",
        rows:[
            getViewHeader(),
            { type:"space", margin:1, padding:1, cols:[
                 { type:"clean" },
                 { width:300, type:"clean", rows:[
                    { type:"clean", height:40, cols:[
                        { view:"search", placeholder:"Search..." },
                        { view:"button", id:"chat_menu_btn", type:"icon", icon:"mail", width:50, onContext:{} }
                    ]},
                    { view:"list", type:"line" }
                 ]},
                 { width:700, rows:[
                       { view:"template", type:"line", height:50 },
                       { view:"scrollview", scroll:"y", type:"clean",
                              body:{
                                  rows:[
                                      { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
                                      { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
                                      { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" }
                                  ]
                              } //end of scrollview body
                          },
                       { type:"clean", rows:[
                            { type:"clean", height:70, cols:[
                                { type:"clean", width:70 },
                                { view:"textarea", placeholder:"Write a message...", height:70 },
                                { type:"clean", width:70 }
                            ]},
                            { type:"clean", height:35, cols:[
                                { type:"clean", width:70 },
                                { view:"button", value:"Send", inputWidth:100 }
                            ]},
                       ]}]
                 },
                 { type:"clean" }
            ]},
            { type:"clean", height:50 }
        ]
    });

    webix.ui({
        view:"contextmenu",
        id:"chat_contextmenu",
        data: [{ value:"New dialog" }]
    });

    $$('chat_contextmenu').attachTo($$('chat_menu_btn').$view);
    $$("chat_contextmenu").attachEvent("onItemClick", function(id){
        webix.ui({ view:"window",
                   height:300,
                   width:400,
                   left:500,
                   top:200,
                   move:true,
                   head:"Start a new conversation",
                   body:{ view:"form", id:"new_dialog_form", labelAlign:"top", elements:[
                            { view:"text", name:"recipient", label:"Recipient", required:true },
                            { view:"textarea", name:"messageText", label:"Message", placeholder:"Write a message...", required:true },
                            { view:"button", value:"Send", type:"form", inputWidth:140, click:"submitNewMessage" }
                   ]}
                }).show();
    });

    submitNewMessage = function(){
        if (this.getParentView().validate()){
        };
    };
})();
