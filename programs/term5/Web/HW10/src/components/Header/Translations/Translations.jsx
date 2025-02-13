import React from 'react';
import logo from "../../../assets/img/logo.png";
import en from "../../../assets/img/gb.png";
import ru from "../../../assets/img/ru.png";

const Translations = () => {
    return (
        <>
            <a href="/">
                <img src={logo} alt="Codeforces" title="Codeforces"/>
            </a>
            <div className="languages">
                <a href="#">
                    <img src={en} alt="In English" title="In English"/>
                </a>
                <a href="#">
                    <img src={ru} alt="In Russian" title="In Russian"/>
                </a>
            </div>
        </>
    );
};

export default Translations;