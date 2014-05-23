exports.install = function(framework) {

    framework.onAuthorization = function(req, res, flags, next) {
        var userId = req.cookie('user');

        if (!userId || userId.length == 0) {
            // unlogged user
            next(false);
            return;
        };

        next(true, {});
    };

    framework.route('/', redirect_login, ['unauthorize']);
    framework.route('/', redirect_chat, ['authorize']);

    framework.route('/login', redirect_chat, ['authorize']);
    framework.route('/login', view_login, ['unauthorize']);
    framework.route('/login', post_login, ['post', 'xhr']);

    framework.route('/chat', redirect_login, ['unauthorize']);
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

    var data = self.req.data.post;
    if (data && data.nickname && data.password){
        self.res.cookie('user', data.nickname)
        redirect_chat();
    }
    self.json({ error: "invalid credentials" });
}

function redirect_login(){
    var self = this;

    self.redirect('/login');
};

function redirect_chat(){
    var self = this;

    self.redirect('/chat');
};