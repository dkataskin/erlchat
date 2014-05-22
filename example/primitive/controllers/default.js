exports.install = function(framework) {
    framework.route('/', view_login);
};
 
// login view
function view_login() {
    var self = this;
    self.view('login');
}