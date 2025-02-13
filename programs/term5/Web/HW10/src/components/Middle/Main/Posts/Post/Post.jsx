import voteup from "../../../../../assets/img/voteup.png";
import votedown from "../../../../../assets/img/votedown.png";
import date_16x16 from "../../../../../assets/img/date_16x16.png"
import comments_16x16 from "../../../../../assets/img/comments_16x16.png";
import Comments from "./Comments/Comments";
import WriteComment from "./Comments/WriteComment/WriteComment";


const Post = ({user, post, setPage, setPageArgs, viewComments, createComment}) => {

    return (
        <article>
            <div className="title">
                <a href="" onClick={(event) => {
                    event.preventDefault()
                    setPageArgs({post: post, viewComments: true})
                    setPage('post')
                }}> {post.title} </a>
            </div>
            <div className="body" style={{whiteSpace: "pre-line"}}>{post.text}</div>

            <div className="footer">
                <div className="left">
                    <img src={voteup} title="Vote Up" alt="Vote Up"/>
                    <span className="positive-score">+173</span>
                    <img src={votedown} title="Vote Down" alt="Vote Down"/>
                </div>
                <div className="right">
                    <img src={date_16x16} title="Publish Time" alt="Publish Time"/>
                    <img src={comments_16x16} title="Comments" alt="Comments"/>
                    <a href="#">{post.comments ? post.comments.length : 0}</a>
                </div>
            </div>
            {viewComments ?
                <>
                  {user ? <WriteComment user={user} createComment={createComment} post={post} setPage={setPage} setPageArgs={setPageArgs} /> : <></>}
                  <Comments comments={post.comments} setPage={setPage}/>
                </>
                :
                <></>}
        </article>

    )
}

export default Post