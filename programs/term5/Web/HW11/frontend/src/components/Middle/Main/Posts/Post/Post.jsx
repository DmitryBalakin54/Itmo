import voteup from "../../../../../assets/img/voteup.png";
import votedown from "../../../../../assets/img/votedown.png";
import date_16x16 from "../../../../../assets/img/date_16x16.png";
import comments_16x16 from "../../../../../assets/img/comments_16x16.png";
import { dateFormat } from "../../../../../utils.js";
import { useEffect, useState } from "react";
import {useNavigate, useParams} from "react-router-dom";
import axios from "axios";

const Post = ({post, isOnlyOne}) => {
    const { id } = useParams();
    const [curPost, setCurPost] = useState(post);
    const router = useNavigate();

    const getPost = async() => {
        axios.get(`/api/posts/${id}`).then((response) => {
            setCurPost(response.data);
        }).catch((error) => console.log(error));
    }

    useEffect(() => {
        if (isOnlyOne) {
            getPost().then(() => {})
        }
    }, [id])

    if (!curPost) {
        router("*")
        return <></>
    }

    return (
        <article>
            <div className="title">
                <a href=""> {curPost.title} </a>
            </div>
            <div className="information">
                By {curPost.user.login }, {dateFormat(curPost.creationTime)}
            </div>

            <div className="body" style={{ whiteSpace: "pre-line" }}>{curPost.text}</div>

            <div className="footer">
                <div className="left">
                    <img src={voteup} title="Vote Up" alt="Vote Up" />
                    <span className="positive-score">+173</span>
                    <img src={votedown} title="Vote Down" alt="Vote Down" />
                </div>
                <div className="right">
                    <img src={date_16x16} title="Publish Time" alt="Publish Time" />
                    <img src={comments_16x16} title="Comments" alt="Comments" />
                    <a href="#">0</a>
                </div>
            </div>
        </article>
    );
};

export default Post;
