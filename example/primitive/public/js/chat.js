(function(){

    webix.ui({ type:"clean",
        rows:[
            getViewHeader(),
            { type:"clean", margin:1, padding:1, cols:[
                 { type:"clean" },
                 { type:"clean", id:"chat_pane", cols:[
                    { width:300, type:"clean", rows:[
                        { type:"clean", height:40, cols:[
                            { view:"search", placeholder:"Search..." },
                            { view:"menu",
                              id:"chat_menu",
                              container:"chat_menu_cntr",
                              width:50,
                              layout:"y",
                              openAction:"click",
                              data: [
                                   { id:1, value:"M", submenu:[ "New conversation" ] }
                              ]}
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
                 ]},
                 { type:"clean" }
            ]},
            { type:"clean", height:50 }
        ]
    });

    $$("chat_menu").attachEvent("onMenuItemClick", function(id){
        if (this.getMenuItem(id).value == "New conversation"){
            webix.message(this.getMenuItem(id).value);
            webix.ui({ view:"window",
                       id:"new_dialog_window",
                       height:300,
                       width:400,
                       left:500,
                       top:200,
                       move:true,
                       head:"Start a new conversation",
                       body:{ view:"form", id:"new_dialog_form", labelAlign:"top", elements:[
                                { view:"text", name:"recipient", label:"Recipient", required:true },
                                { view:"textarea", name:"messageText", label:"Message", placeholder:"Write a message...", required:true },
                                { type:"clear", margin:10, cols:[
                                    { view:"button", value:"Send", type:"form", click:"submitNewMessage" },
                                    { view:"button", value:"Cancel", click:"$$('new_dialog_window').close();" }
                                ]}
                       ]}
                    }).show();
        };
    });

    submitNewMessage = function(){
        if (this.getParentView().validate()){
        };
    };
})();
