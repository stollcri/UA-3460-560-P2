��    0      �  C         (     )     E  !   ^     �     �     �  &   �     �     �  �   
  �   �  �   �  	   C     M     Y     e     n     z     �     �  ;  �     �     �  "   �  �   	     �	     �	     �	  �   �	  �   �
       1         R     [     d  �   �  B   _  1   �     �  5   �     #     )     7  �  C  )   �            �    $   �     �  4     #   P  )   t  !   �  ;   �  
   �       �       �    �                    ,     5  	   E     O  
   U  �  `  
   �       :       P     n     s       �   �  �   y       8   "  	   [  
   e  #   p  1  �  ]   �  *   $  #   O  h   s     �     �     �  �     9   �&     %'     ,'               (          &                                             /   )      +   
       	      $      -   #          "                   !                .                      %                       *           ,   '            0    %s \- manual page for %s %s %s: can't create %s (%s) %s: can't get `%s' info from %s%s %s: can't open `%s' (%s) %s: can't unlink %s (%s) %s: error writing to %s (%s) %s: no valid information found in `%s' AUTHOR AVAILABILITY Additional material may be included in the generated output with the
.B \-\-include
and
.B \-\-opt\-include
options.  The format is simple:

    [section]
    text

    /pattern/
    text
 Any
.B [NAME]
or
.B [SYNOPSIS]
sections appearing in the include file will replace what would have
automatically been produced (although you can still override the
former with
.B --name
if required).
 Blocks of verbatim *roff text are inserted into the output either at
the start of the given
.BI [ section ]
(case insensitive), or after a paragraph matching
.BI / pattern /\fR.
 COPYRIGHT DESCRIPTION ENVIRONMENT EXAMPLES Environment Examples FILES Files GNU %s %s

Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2009, 2010
Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Written by Brendan O'Dea <bod@debian.org>
 Games INCLUDE FILES Include file for help2man man page Lines before the first section or pattern which begin with `\-' are
processed as options.  Anything else is silently ignored and may be
used for comments, RCS keywords and the like.
 NAME OPTIONS Options Other sections are prepended to the automatically produced output for
the standard sections given above, or included at
.I other
(above) in the order they were encountered in the include file.
 Patterns use the Perl regular expression syntax and may be followed by
the
.IR i ,
.I s
or
.I m
modifiers (see
.BR perlre (1)).
 REPORTING BUGS Report +(?:[\w-]+ +)?bugs|Email +bug +reports +to SEE ALSO SYNOPSIS System Administration Utilities The full documentation for
.B %s
is maintained as a Texinfo manual.  If the
.B info
and
.B %s
programs are properly installed at your site, the command
.IP
.B info %s
.PP
should give you access to the complete manual.
 The latest version of this distribution is available on-line from: The section output order (for those included) is: This +is +free +software Try `--no-discard-stderr' if option outputs to stderr Usage User Commands Written +by `%s' generates a man page out of `--help' and `--version' output.

Usage: %s [OPTION]... EXECUTABLE

 -n, --name=STRING       description for the NAME paragraph
 -s, --section=SECTION   section number for manual page (1, 6, 8)
 -m, --manual=TEXT       name of manual (User Commands, ...)
 -S, --source=TEXT       source of program (FSF, Debian, ...)
 -L, --locale=STRING     select locale (default "C")
 -i, --include=FILE      include material from `FILE'
 -I, --opt-include=FILE  include material from `FILE' if it exists
 -o, --output=FILE       send output to `FILE'
 -p, --info-page=TEXT    name of Texinfo manual
 -N, --no-info           suppress pointer to Texinfo manual
     --help              print this help, then exit
     --version           print version number, then exit

EXECUTABLE should accept `--help' and `--version' options and produce output on
stdout although alternatives may be specified using:

 -h, --help-option=STRING     help option string
 -v, --version-option=STRING  version option string
 --version-string=STRING      version string
 --no-discard-stderr          include stderr when parsing option output

Report bugs to <bug-help2man@gnu.org>.
 help2man \- generate a simple manual page or other Project-Id-Version: help2man 1.38.1-pre1
Report-Msgid-Bugs-To: Brendan O'Dea <bug-help2man@gnu.org>
POT-Creation-Date: 2010-04-26 15:18+1000
PO-Revision-Date: 2010-04-13 20:28+0930
Last-Translator: Clytie Siddall <clytie@riverland.net.au>
Language-Team: Vietnamese <vi-VN@googlegroups.com>
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=1; plural=0;
X-Generator: LocFactoryEditor 1.8
 %s \- trang hướng dẫn cho %s %s %s: không thể tạo %s (%s) %s: không thể lấy thông tin « %s » từ %s%s %s: không thể mở « %s » (%s) %s: không thể bỏ liên kết %s (%s) %s:  lỗi ghi nhớ vào %s (%s) %s: không tìm thấy thông tin hợp lệ trong « %s » TÁC GIẢ TÍNH SẴN SÀNG Cũng có thể bao gồm dữ liệu bổ sung trong kết xuất, dùng tuỳ chọn
.B \-\-include
và
.B \-\-opt\-include
Có một định dạng đơn giản:

    [phần]
    chuỗi

    /mẫu/
    chuỗi
 Bắt cứ phần
.B [NAME]
hoặc
.B [SYNOPSIS]
nào xuất hiện trong tập tin bao gồm thì thay thế kết xuất tự động tạo
(dù bạn vẫn còn có dịp ghi đè lên phần [NAME] bằng:
.B --name
nếu cần).

NAME: TÊN
SYNOPSIS: TÓM TẮT
 Khối văn bản định dạng *roff đúng nguyên văn được chèn vào kết xuất
hoặc ở đầu của phần
.BI [ phần ]
(không phân biệt chữ hoa/thường),
hoặc đẳng sau một đoạn văn tương ứng với mẫu
.BI / mẫu /\fR.
 BẢN QUYỀN MÔ TẢ MÔI TRƯỜNG VÍ DỤ Môi +trường Ví +dụ TỆP Tập +tin GNU %s %s

Tác quyền © năm 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2009, 2010
của Tổ chức Phần mềm Tự do.
Đây là phần mềm tự do; hãy xem mã nguồn để biết về điều kiện sao chép.
KHÔNG có sự đảm bảo nào; THẬM CHÍ KHÔNG CÓ KHẢ NĂNG THƯƠNG MẠI
HAY CHO MỘT MỤC ĐÍCH RIÊNG NÀO ĐÓ.

Viết bởi Brendan O'Dea <bod@debian.org>
 Trò chơi TỆP BAO GỒM Bao gồm tập tin cho trang hướng dẫn về help2man Dòng nào đẳng trước phần đầu tiên hoặc mẫu bắt đầu với « \- » thì được xử
lý dưới dạng tuỳ chọn. Dữ liệu khác (nếu có) bị bỏ qua mà không xuất chi
tiết, và có thể được sử dụng làm ghi chú, từ khoá RCS v.v.
 TÊN TÙY CHỌN Tuỳ +chọn Các phần khác được thêm vào đầu của kết xuất tự động tạo
cho những phần tiêu chuẩn đưa ra trên, hoặc được bao gồm tại
.I other
(bên trên) theo thứ tự gặp trong tập tin bao gồm.
 Mẫu chấp nhận cú pháp của biểu thức chính quy Perl,
và cũng cho phép dấu sửa đổi
.IR i ,
.I s
hay
.I m
(xem
.BR perlre (1)).
 THÔNG BÁO LỖI Thông +báo +lỗi|Gửi thư +thông +báo +lỗi +cho XEM THÊM TÓM TẮT Tiện ích quản lý hệ thống Tài liệu hướng dẫn đầy đủ về
.B %s
được bảo tồn dưới dạng một sổ tay Texinfo.
Nếu chương trình
.B info
và
.B %s
được cài đặt đúng ở địa chỉ của bạn thì câu lệnh
.IP
.B info %s
.PP
nên cho phép bạn truy cập đến sổ tay hoàn toàn.
 Phiên bản mới nhất của bản phân phối này có sẵn sàng trực tuyến từ : Thứ tự xuất phần (đã bao gồm): Đây +là +phần +mềm +tự +do Nếu tuỳ chọn xuất qua đầu lỗi tiêu chuẩn thì thử lập cờ « --no-discard-stderr » Sử dụng Lệnh người dùng Viết +bởi « %s » tạo một trang hướng dẫn dựa vào kết xuất của tuỳ chọn
`--help' (trợ giúp) và `--version' (phiên bản).

Sử dụng: %s [TÙY_CHỌN]... BẢN_THỰC_HIỆN_ĐƯỢC

 -n, --name=CHUỖI       mô tả cho đoạn văn TÊN (NAME)
 -s, --section=PHẦN     số thứ tự phần cho trang hướng dẫn (1, 6, 8)
 -m, --manual=TÊN       tên của trang hướng dẫn (User Commands, ...)
 -S, --source=TEXT      nguồn của chương trình (FSF, Debian, ...)
 -L, --locale=CHUỖI     chọn miền địa phương (mặc định "C";
                        đối với tiếng Việt thì là « vi_VN.UTF-8 »)
 -i, --include=TỆP      bao gồm dữ liệu từ tập tin này
 -I, --opt-include=TỆP  bao gồm dữ liệu từ tập tin này nếu có
 -o, --output=TỆP       gửi kết xuất cho tập tin này
 -p, --info-page=TÊN    tên của trang hướng dẫn loại Texinfo
 -N, --no-info          thu hồi cái chỉ tới trang hướng dẫn Texinfo
     --help             hiển thị trợ giúp này, sau đó thoát
     --version          hiển thị số thứ tự phiên bản, sau đó thoát

BẢN_THỰC_HIỆN_ĐƯỢC nên chấp nhận hai tuỳ chọn `--help' và `--version'
thì xuất qua đầu ra tiêu chuẩn (stdout) dù cũng có thể ghi rõ thêm tuỳ chọn:

 -h, --help-option=CHUỖI     chuỗi là tuỳ chọn trợ giúp
 -v, --version-option=CHUỖI  chuỗi là tuỳ chọn phiên bản
 --version-string=CHUỖI      chuỗi phiên bản
 --no-discard-stderr         bao gồm đầu lỗi tiêu chuẩn
                             khi phân tích cú pháp của kết xuất tuỳ chọn

Thông báo lỗi nào cho <bug-help2man@gnu.org>.
 help2man \- tạo một trang hướng dẫn đơn giản hoặc khác 