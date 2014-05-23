exports.install = function(framework) {

    framework.onAuthorization = function(req, res, flags, next) {
        var userId = req.cookie('user').parseInt();

        if (userId === 0) {
            // unlogged user
            next(false);
            return;
        };

        next(true, {});
    };

    framework.route('/', redirect_login);
    framework.route('/', redirect_chat, ['authorize']);

    framework.route('/login', view_login);
    framework.route('/login', post_login, ['post', 'xhr']);

    framework.route('/chat', redirect_login);
    framework.route('/chat', view_chat, ['authorize']);
};
 
// login view
function view_login() {
    var self = this;
    self.view('login');
}

// chat view
function view_chat() {
    var self = this;
    self.view('chat');
}

function post_login() {
    var self = this;

    self.redirect('/chat');
}

function redirect_login(){
    var self = this;

    self.redirect('/login');
};

function redirect_chat(){
    var self = this;

    self.redirect('/login');
};