<#-- @ftlvariable name="post" type="ru.itmo.wp.domain.Post" -->
<#-- @ftlvariable name="comments" type="ru.itmo.wp.domain.Comment[]" -->
<#-- @ftlvariable name="comment" type="ru.itmo.wp.domain.Comment" -->

<#import "commons.ftlh" as c>

<@c.page>
    <#if post??>
        <@c.post post=post/>
        <#if user??>
            <div class="form">
                <div class="header">Write Comment</div>
                <div class="body">
                    <form method="post" action="">
                        <input type="hidden" name="action" value="writeComment"/>
                        <div class="field">
                            <div class="value">
                                <textarea class="comment" id="text" name="text" rows="3">${comment.text!}</textarea>
                            </div>
                            <@c.error "comment.text"/>
                        </div>
                        <div class="button-field" style="padding-bottom: 1rem">
                            <input type="submit" value="Send">
                        </div>
                    </form>
                </div>
            </div>
        </#if>
        <#list comments as comment>
            <@c.comment comment=comment/>
        </#list>
    <#else>
        No such post.
    </#if>
</@c.page>