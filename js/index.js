function map(Obj) {
    return {t: "map", v: Object.entries(Obj).map(([k, v]) => tuple([k, v]))};
}

function tuple(ElementList) {
    if (Array.isArray(ElementList)) {
	return {t: "tuple", v: ElementList};
    } else {
	throw new Error(`tuple only accept array, but got: ${ElementList}`);
    }
}

function atom(V) {
    if (typeof V === "string") {
	return {t: "atom", v: V};
    } else {
	throw new Error(`atom only accept string, but got: ${V}`);
    }
}


function decode(Obj) {
    if (Array.isArray(Obj)) {
	if (Obj.length > 0) {
	    return [decode(Obj[0])].concat(decode(Obj.slice(1)));
	} else {
	    return [];
	}
    } else if (typeof Obj === "object") {
	if (!Obj.t) {
	    throw new Error(`invalid serval object ${Obj}`);
	}
	if (Obj.t === "tuple") {
	    return decode(Obj.v);
	} else if (Obj.t === "map") {
	    return Object.fromEntries(decode(Obj.v));
	} else {
	    return Obj.v;
	}
    } else if (typeof Obj === "string") {
	return Obj;
    } else if (typeof Obj === "number") {
	return Obj;
    } else {
	throw new Error(`invalid serval object (${Obj})`);
    }
}

module.exports = {
    map,
    tuple,
    atom,
    decode,
};
