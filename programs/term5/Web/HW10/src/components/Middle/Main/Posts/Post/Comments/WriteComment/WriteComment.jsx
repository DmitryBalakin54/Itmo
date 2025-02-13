import React, {useRef, useState} from 'react';

const WriteComment = ({user, createComment, post, setPage, setPageArgs}) => {

    const textInputRef = useRef(null)
    const [error, setError] = useState('')

    const handleComment = (event) => {
        event.preventDefault()
        const text = textInputRef.current.value
        if (text.trim().length === 0) {
            setError('Text can not be empty')
            return
        }
        createComment({
            author: user.login,
            text: text,
            postId: post.id
        })

        textInputRef.current.value = ''
        setPageArgs({post: post, viewComments: true})
        setPage('post')
    }

    return (
        <div className="form">
            <div className="header">Write Comment</div>
            <div className="body">
                <form method="post" action="" onSubmit={handleComment}>
                    <input type="hidden" name="action" value="writeComment"/>
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
                        <input type="submit" value="Comment"/>
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

export default WriteComment;