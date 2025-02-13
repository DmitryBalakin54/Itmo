<#-- @ftlvariable name="postParams" type="ru.itmo.wp.form.WritePostForm" -->


<#import "/spring.ftl" as spring>
<#import "commons.ftlh" as c>

<@c.page>
    <div class="form">
        <div class="header">Write Post</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="writePost"/>
                <div class="field">
                    <div class="name">
                        <label for="title">Title</label>
                    </div>
                    <div class="value">
                        <input autofocus id="title" name="title" value="${postParams.title!}"/>
                    </div>
                    <@c.error "postParams.title"/>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text">${postParams.text!}</textarea>
                    </div>
                    <@c.error "postParams.text"/>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="tagsString">Tags</label>
                    </div>
                    <div class="value">
                        <input id="tagsString" name="tagsString" value="${postParams.tagsString!}"/>
                    </div>
                    <@c.error "postParams.tagsString"/>
                </div>
                <div class="button-field">
                    <input type="submit" value="Write">
                </div>
            </form>
        </div>
    </div>
</@c.page>
