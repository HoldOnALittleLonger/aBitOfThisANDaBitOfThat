#include <stdio.h>
#include <string.h>

int main(void)
{
        /**
         * @self - this const char pointer points to the content of this file,
         *         we need to printf this string.
         *         its format is :
         *                 ForwardPart
         *                 const char *self = "ForwardPart @self BackwardPart";
         *                 BackwardPart
         *         but there is a problem,the string is depends to itself recursively.
         *         if we want to define such string,we need to contain itself into
         *         its content.
         *         if we need to contain itself into its content,the content will
         *         contains the definition of itself.
         *         we need a way to break this loop.
         *         so,we introduced a ReplacePoint,if we encountered this ReplacePoint,
         *         we need use @self to replace it.
         *         the replace can be deferred,that is,we do not apply the replacement
         *         into the definition of @self,instead,we mark the point,and when
         *         we print @self,if we encountered the mark,we print @self again.
         *         in the definition,we used a mark,but we can not write it at there
         *         because that will cause print @self again.we print @self content
         *         through standard library function,but do not interpret it to a
         *         string,because '%s' will convert escape symbols.
         *         so,the format will be :
         *                 ForwardPart
         *                 @self = ForwardPart Mark BackwardPart
         *                                     |
         *                                     +--> ForwardPart
         *                                          @self = ForwardPart Mark BackwardPart
         *                                                              ...
         *                                          BackwardPart
         *                 BackwardPart
         *         
         *                 const char *self = "@TheMark";
         *                 
         *                 Then we makeup the string,and use the string to replace
         *                 @TheMark,we finally get it.
         *
         *                 const char *self = "<ForwardPart><@Mark><BackwardPart>";
         *
         *         because we only print @self more once,thus the ForwardPart and
         *         BackwardPart should not contains Mark,we can use ASCII number to
         *         complete the testing.
         *         # we only need to print this file,do not care about standard library
         *           function's content.
         *         # we need to print escape characters '\n', '"', '\',thus we need 
         *           do escape for them at first,let the converted content become the
         *           @self.
         *           escape sequences :
         *                   '\n' => '\\n'  => '\n'
         *                   '"'  => '\\\"' => '\"'
         *                   '\\' => '\\\\' => '\\'
         *           # '\' also need to be converted again to the final sequence.
         *         # the diff @output @source_file should prints nothing.
         */
        const char *self = "#include <stdio.h>\n#include <string.h>\n\nint main(void)\n{\n        /**\n         * @self - this const char pointer points to the content of this file,\n         *         we need to printf this string.\n         *         its format is :\n         *                 ForwardPart\n         *                 const char *self = \"ForwardPart @self BackwardPart\";\n         *                 BackwardPart\n         *         but there is a problem,the string is depends to itself recursively.\n         *         if we want to define such string,we need to contain itself into\n         *         its content.\n         *         if we need to contain itself into its content,the content will\n         *         contains the definition of itself.\n         *         we need a way to break this loop.\n         *         so,we introduced a ReplacePoint,if we encountered this ReplacePoint,\n         *         we need use @self to replace it.\n         *         the replace can be deferred,that is,we do not apply the replacement\n         *         into the definition of @self,instead,we mark the point,and when\n         *         we print @self,if we encountered the mark,we print @self again.\n         *         in the definition,we used a mark,but we can not write it at there\n         *         because that will cause print @self again.we print @self content\n         *         through standard library function,but do not interpret it to a\n         *         string,because '%s' will convert escape symbols.\n         *         so,the format will be :\n         *                 ForwardPart\n         *                 @self = ForwardPart Mark BackwardPart\n         *                                     |\n         *                                     +--> ForwardPart\n         *                                          @self = ForwardPart Mark BackwardPart\n         *                                                              ...\n         *                                          BackwardPart\n         *                 BackwardPart\n         *         \n         *                 const char *self = \"@TheMark\";\n         *                 \n         *                 Then we makeup the string,and use the string to replace\n         *                 @TheMark,we finally get it.\n         *\n         *                 const char *self = \"<ForwardPart><@Mark><BackwardPart>\";\n         *\n         *         because we only print @self more once,thus the ForwardPart and\n         *         BackwardPart should not contains Mark,we can use ASCII number to\n         *         complete the testing.\n         *         # we only need to print this file,do not care about standard library\n         *           function's content.\n         *         # we need to print escape characters '\\n', '\"', '\\',thus we need \n         *           do escape for them at first,let the converted content become the\n         *           @self.\n         *           escape sequences :\n         *                   '\\n' => '\\\\n'  => '\\n'\n         *                   '\"'  => '\\\\\\\"' => '\\\"'\n         *                   '\\\\' => '\\\\\\\\' => '\\\\'\n         *           # '\\' also need to be converted again to the final sequence.\n         *         # the diff @output @source_file should prints nothing.\n         */\n        const char *self = \"?\";\n        for (unsigned i = 0; i < strlen(self); ++i) {\n                if (self[i] == 63)\n                        for (unsigned j = 0; j < strlen(self); ++j)\n                                switch (self[j]) {\n                                case '\\n':\n                                        printf(\"\\\\n\");\n                                        break;\n                                case '\"':\n                                        printf(\"\\\\\\\"\");\n                                        break;\n                                case '\\\\':\n                                        printf(\"\\\\\\\\\");\n                                        break;\n                                default:\n                                        printf(\"%c\", self[j]);\n                                }\n                else\n                        printf(\"%c\", self[i]);\n\n        }\n        return 0;\n}\n";
        for (unsigned i = 0; i < strlen(self); ++i) {
                if (self[i] == 63)
                        for (unsigned j = 0; j < strlen(self); ++j)
                                switch (self[j]) {
                                case '\n':
                                        printf("\\n");
                                        break;
                                case '"':
                                        printf("\\\"");
                                        break;
                                case '\\':
                                        printf("\\\\");
                                        break;
                                default:
                                        printf("%c", self[j]);
                                }
                else
                        printf("%c", self[i]);

        }
        return 0;
}
