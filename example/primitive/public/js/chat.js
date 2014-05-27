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
                    { view:"template", type:"line" }
                 ]},
                 { width:700, rows:[
                       { view:"template", type:"line", height:50 },
                       { view:"scrollview", scroll:"y",
                              body:{
                                  rows:[
                                      { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
                                      { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
                                      { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
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
})();