function clickHandler (owner) {
    title = owner.getElementsByTagName("title")[0].textContent;
    location.href = "click:" + title;
}

