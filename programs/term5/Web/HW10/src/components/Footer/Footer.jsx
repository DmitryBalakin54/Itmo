import React from 'react';

const Footer = ({usersCount, postsCount}) => {
    return (
        <footer>
            <a href="#">Codehorses</a> 2099 by Mike Mirzayanov

            <p>Total users: {usersCount}</p>
            <p>Total posts: {postsCount}</p>
        </footer>
    );
};

export default Footer;