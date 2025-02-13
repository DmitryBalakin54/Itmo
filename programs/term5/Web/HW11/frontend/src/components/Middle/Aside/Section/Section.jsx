import React from 'react';
import {useNavigate} from "react-router-dom";

const Section = ({post}) => {
    const router = useNavigate();
    return (
        <section>
            <div className="header">
                {post.title}
            </div>
            <div className="body">
                {post.text}
            </div>
            <div className="footer">
                <a href="" onClick={(event) => {
                    event.preventDefault()
                    router('/posts/' + post.id)
                }}>View all</a>
            </div>
        </section>
    );
};

export default Section;