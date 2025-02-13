import React from 'react';

const Section = ({post, setPage, setPageArgs}) => {
    return (
        <section>
            <div className="header" >
                {post.title}
            </div>
            <div className="body">
                {post.text}
            </div>
            <div className="footer">
                <a href="#" onClick={(event) => {
                    event.preventDefault()
                    setPageArgs({post: post, viewComments: true})
                    setPage('post')
                }}>View all</a>
            </div>
        </section>
    );
};

export default Section;