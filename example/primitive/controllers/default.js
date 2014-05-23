exports.install = function(framework) {

    framework.onAuthorization = function(req, res, flags, next) {
        var sessionToken = req.cookie('session');

        if (!sessionToken || sessionToken.length == 0) {
            // unlogged user
            next(false);
            return;
        };

        var session = framework.cache.read(sessionToken);
        if (!session){
            console.log("Session " + sessionToken + " not found.");

            next(false);
            return;
        }

        next(true, session);
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
        var sessionToken = generateSessionToken(data.nickname, data.password);
        framework.cache.add(sessionToken, { username : data.nickname, token : sessionToken }, new Date().add('hour', 1));

        console.log("User " + data.nickname + " logged in, session token: " + sessionToken)

        self.res.cookie('session', sessionToken)
        self.redirect('/chat');

        return;
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

function generateSessionToken(username, password){
    var crypto = require('crypto');
    var md5sum = crypto.createHash('md5');
    md5sum.update(username + ":" + password);

    return md5sum.digest('hex');
};
