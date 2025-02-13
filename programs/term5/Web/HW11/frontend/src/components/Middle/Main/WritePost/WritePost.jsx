import React, {useCallback, useRef, useState} from 'react';
import {useNavigate} from "react-router-dom";
import axios from "axios";

const WritePost = ({posts, setPosts, userLogin}) => {

    const titleInputRef = useRef(null)
    const textInputRef = useRef(null)
    const [error, setError] = useState('')

    const router = useNavigate()

    const handleSubmit = useCallback(() => {
        const title = titleInputRef.current.value
        const text = textInputRef.current.value
        if (title.trim().length === 0 || text.trim().length === 0) {
            setError('Title or text can not be empty')
            return
        }

        axios.post("/api/posts", {
            title: title,
            text: text,
            userLogin: userLogin,
        }).then((response) => {
            setPosts([...posts, response.data])
        }).catch((error) => {
            setError(error.response.data)
        })

        router("/")
    })

    return (
        <div className="form">
            <div className="header">Write Post</div>
            <div className="body">
                <form method="" action="" onSubmit={event => {
                    event.preventDefault()
                    handleSubmit()
                }}>
                    <input type="hidden" name="action" value="writePost"/>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="title">Title</label>
                        </div>
                        <div className="value">
                            <input
                                autoFocus
                                id="title"
                                name="title"
                                ref={titleInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="text">Text</label>
                        </div>
                        <div className="value">
                            <textarea
                                id="text"
                                name="text"
                                ref={textInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    <div className="button-field">
                        <input type="submit" value="Write"/>
                    </div>
                    {error
                        ? <div className={'error'}>{error}</div>
                        : null
                    }
                </form>
            </div>
        </div>
    );
};

export default WritePost;