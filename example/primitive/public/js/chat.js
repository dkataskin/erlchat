(function(){
    webix.ui({
        rows:[
            getViewHeader(),
            { cols:[
                 { type:"clean" },
                 { width:300, type:"clean", rows:[
                    { view:"template", height:50 },
                    { view:"template" }
                 ]},
                 { width:700, rows:[
                       { view:"layout", type:"line", rows:[
                               { template:"<span style='font-size:16px; text-align:center; font-weight:bold'>John Keats\' Verses</span>", height:50},
                               { view:"scrollview", id:"verses", scroll:"y",
                                   body:{
                                       rows:[
                                           { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
                                           { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
                                           { template:"<i>Scroll down for the next verse</i>", height:30, type:"clean" },
                                       ]
                                   } //end of scrollview body
                               }
                           ]
                       }]
                 },
                 { type:"clean" }
            ]},
            { type:"clean", height:50 }
        ]
    });
})();