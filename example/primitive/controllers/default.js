exports.install = function(framework) {
    framework.route('/', view_login);
    framework.route('/chat', view_chat);
};
 
// login view
function view_login() {
    var self = this;
    self.view('login');
}

function view_chat() {
    var self = this;
    self.view('chat');
}