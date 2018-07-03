const start2 = pattern({
	'x,y': function(x,y) {
		const f = function(a) {
		return a.log('100');
	};
		f(console);
		return (pattern({
	'1': function() {
		return console.log([x,y]);
	},
	'_': function() {
		return console.log('ok');
	}})
)(x);
	}});
const start = pattern({
	'': function() {
		h1({id: 'myproduct_id'});
		start2(1,3);
		const j = 5;
		const n = fac(j);
		return console.log('factorial ~p',[j,n]);
	}});
const fac = pattern({
	'0': function(_0) {
		return 1;
	},	'n': function(n) {
		return n * fac(n - 1);
	}});

start();
