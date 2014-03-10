@implementation Environment

- alloc do
    @throw [Exception createWithMessage:"Cannot create an instance of Environment"]
end

+(String) NewLine do
    if [Environment Platform] == @'Windows' then
        return @"\r\n"
    else
        return @"\n"
    end
end

+(String) Platform do
    -- TODO: auto-configure
    return [String createWithString:'Windows']
end

@end
